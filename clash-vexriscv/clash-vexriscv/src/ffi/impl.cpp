// SPDX-FileCopyrightText: 2022 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "VVexRiscv.h"
#include "interface.h"
#include "verilated.h"
#include <verilated_vcd_c.h>

#include <arpa/inet.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>

typedef struct {
  int server_socket, client_handle;
  struct sockaddr_in server_addr;
  struct sockaddr_storage server_storage;

  uint32_t timer;
  socklen_t addr_size;
  uint32_t self_sleep;
  uint32_t check_new_connections_timer;
  uint8_t rx_buffer[100];
  int32_t rx_buffer_size;
  int32_t rx_buffer_remaining;
  JTAG_INPUT prev_input;
} vexr_jtag_bridge_data;

extern "C" {
VVexRiscv *vexr_init();
VerilatedVcdC *vexr_init_vcd(VVexRiscv *top, const char *path);
void vexr_shutdown(VVexRiscv *top);

void vexr_init_stage1(VerilatedVcdC *vcd, uint64_t dom_period_fs,
                      VVexRiscv *top, const NON_COMB_INPUT *input,
                      OUTPUT *output);
void vexr_init_stage2(VVexRiscv *top, const COMB_INPUT *input);
void vexr_step_rising_edge(VerilatedVcdC *vcd, VVexRiscv *top,
                           uint64_t time_add, const NON_COMB_INPUT *input,
                           OUTPUT *output);
void vexr_step_falling_edge(VerilatedVcdC *vcd, VVexRiscv *top,
                            uint64_t time_add, const COMB_INPUT *input);

vexr_jtag_bridge_data *vexr_jtag_bridge_init(uint16_t port);
void vexr_jtag_bridge_step(vexr_jtag_bridge_data *d, const JTAG_OUTPUT *output,
                           JTAG_INPUT *input);
void vexr_jtag_bridge_shutdown(vexr_jtag_bridge_data *bridge_data);
}

static VerilatedContext *contextp = 0;
static bool set_socket_blocking_enabled(int fd, bool blocking);
static void connection_reset(vexr_jtag_bridge_data *bridge_data);

VVexRiscv *vexr_init() {
  contextp = new VerilatedContext;
  VVexRiscv *v = new VVexRiscv(contextp);
  v->clk = false;
  return v;
}

VerilatedVcdC *vexr_init_vcd(VVexRiscv *top, const char *path) {
  VerilatedVcdC *vcd = new VerilatedVcdC;
  Verilated::traceEverOn(true);
  // Trace 99 levels of the hierarchy. We only have one level AFAIK, so this
  // should be enough :-).
  top->trace(vcd, 99);
  vcd->open(path);
  return vcd;
}

// Set all inputs that cannot combinationaly depend on outputs. I.e., all inputs
// except the Wishbone buses.
void set_non_comb_inputs(VVexRiscv *top, const NON_COMB_INPUT *input) {
  top->reset = input->reset;
  top->timerInterrupt = input->timerInterrupt;
  top->externalInterrupt = input->externalInterrupt;
  top->softwareInterrupt = input->softwareInterrupt;
}

// Set all inputs that can combinationaly depend on outputs. I.e., the Wishbone
// buses.
void set_comb_inputs(VVexRiscv *top, const COMB_INPUT *input) {
  top->iBusWishbone_ACK = input->iBusWishbone_ACK;
  top->iBusWishbone_DAT_MISO = input->iBusWishbone_DAT_MISO;
  top->iBusWishbone_ERR = input->iBusWishbone_ERR;
  top->dBusWishbone_ACK = input->dBusWishbone_ACK;
  top->dBusWishbone_DAT_MISO = input->dBusWishbone_DAT_MISO;
  top->dBusWishbone_ERR = input->dBusWishbone_ERR;

  top->jtag_tck = input->jtag_TCK;
  top->jtag_tms = input->jtag_TMS;
  top->jtag_tdi = input->jtag_TDI;
}

// Set all outputs
void set_outputs(VVexRiscv *top, OUTPUT *output) {
  output->iBusWishbone_CYC = top->iBusWishbone_CYC;
  output->iBusWishbone_STB = top->iBusWishbone_STB;
  output->iBusWishbone_WE = top->iBusWishbone_WE;
  output->iBusWishbone_ADR = top->iBusWishbone_ADR;
  output->iBusWishbone_DAT_MOSI = top->iBusWishbone_DAT_MOSI;
  output->iBusWishbone_SEL = top->iBusWishbone_SEL;
  output->iBusWishbone_CTI = top->iBusWishbone_CTI;
  output->iBusWishbone_BTE = top->iBusWishbone_BTE;
  output->dBusWishbone_CYC = top->dBusWishbone_CYC;
  output->dBusWishbone_STB = top->dBusWishbone_STB;
  output->dBusWishbone_WE = top->dBusWishbone_WE;
  output->dBusWishbone_ADR = top->dBusWishbone_ADR;
  output->dBusWishbone_DAT_MOSI = top->dBusWishbone_DAT_MOSI;
  output->dBusWishbone_SEL = top->dBusWishbone_SEL;
  output->dBusWishbone_CTI = top->dBusWishbone_CTI;
  output->dBusWishbone_BTE = top->dBusWishbone_BTE;

  output->ndmreset = top->ndmreset;
  output->stoptime = top->stoptime;
  output->jtag_TDO = top->jtag_tdo;
}

void vexr_init_stage1(VerilatedVcdC *vcd, uint64_t dom_period_fs,
                      VVexRiscv *top, const NON_COMB_INPUT *input,
                      OUTPUT *output) {
  // Set all inputs that cannot combinationaly depend on outputs. I.e., all
  // inputs except the Wishbone buses.
  set_non_comb_inputs(top, input);

  // Combinatorially respond to the inputs
  top->eval();
  if (vcd != NULL) {
    vcd->dump(contextp->time());
  }
  set_outputs(top, output);

  // Advance time by the period of the vexrisc clock in femtoseconds.
  // Because verilator does not support femtosecond resolution, the simulation
  // time is off by a factor of 1000.
  contextp->timeInc(dom_period_fs);
}

void vexr_init_stage2(VVexRiscv *top, const COMB_INPUT *input) {
  set_comb_inputs(top, input);
}

void vexr_shutdown(VVexRiscv *top) {
  delete top;
  delete contextp;
  contextp = 0;
}

void vexr_step_rising_edge(VerilatedVcdC *vcd, VVexRiscv *top,
                           uint64_t time_add, const NON_COMB_INPUT *input,
                           OUTPUT *output) {
  // Advance time since last event. Note that this is 0 for the first call to
  // this function. To get a sensisble waveform, vexr_init has already advanced
  // time.
  contextp->timeInc(time_add); // XXX: time_add is in femtoseconds, timeinc
                               // expects picoseconds

  // docssss
  set_non_comb_inputs(top, input);

  top->clk = true;
  top->eval();
  if (vcd != NULL) {
    vcd->dump(contextp->time());
  }

  // Set all outputs
  set_outputs(top, output);
}

void vexr_step_falling_edge(VerilatedVcdC *vcd, VVexRiscv *top,
                            uint64_t time_add, const COMB_INPUT *input) {
  // advance time since last event
  contextp->timeInc(
      time_add); // time_add is in femtoseconds, timeinc expects picoseconds

  // Update inputs
  top->clk = false;
  set_comb_inputs(top, input);

  // Evaluate the simulation
  top->eval();
  if (vcd != NULL) {
    vcd->dump(contextp->time());
  }
}

vexr_jtag_bridge_data *vexr_jtag_bridge_init(uint16_t port) {
  vexr_jtag_bridge_data *d = new vexr_jtag_bridge_data;

  d->prev_input = {0, 0, 0};

  d->timer = 0;
  d->self_sleep = 0;
  d->check_new_connections_timer = 0;
  d->rx_buffer_size = 0;
  d->rx_buffer_remaining = 0;

  d->server_socket = socket(PF_INET, SOCK_STREAM, 0);
  assert(d->server_socket != -1);
  int flag = 1;
  setsockopt(d->server_socket, /* socket affected */
             IPPROTO_TCP,      /* set option at TCP level */
             TCP_NODELAY,      /* name of option */
             (char *)&flag,    /* the cast is historical cruft */
             sizeof(int)       /* length of option value */
  );

  set_socket_blocking_enabled(d->server_socket, 0);

  d->server_addr.sin_family = AF_INET;
  d->server_addr.sin_port = htons(port);
  d->server_addr.sin_addr.s_addr = inet_addr("127.0.0.1");
  memset(d->server_addr.sin_zero, '\0', sizeof(d->server_addr.sin_zero));

  bind(d->server_socket, (struct sockaddr *)&d->server_addr,
       sizeof(d->server_addr));

  listen(d->server_socket, 1);

  d->client_handle = -1;
  d->addr_size = sizeof(d->server_storage);

  return d;
}

void vexr_jtag_bridge_step(vexr_jtag_bridge_data *d, const JTAG_OUTPUT *output,
                           JTAG_INPUT *input) {
  *input = d->prev_input;

  if (d->timer != 0) {
    d->timer -= 1;
    return;
  }

  d->check_new_connections_timer++;
  if (d->check_new_connections_timer == 200) {
    d->check_new_connections_timer = 0;
    int new_client_handle = accept(
        d->server_socket, (struct sockaddr *)&d->server_storage, &d->addr_size);
    if (new_client_handle != -1) {
      if (d->client_handle != -1) {
        connection_reset(d);
      }
      d->client_handle = new_client_handle;
    } else if (d->client_handle == -1) {
      d->self_sleep = 200;
    }
  }

  if (d->self_sleep) {
    d->self_sleep--;
    return;
  }

  if (d->client_handle != -1) {
    int n;

    if (d->rx_buffer_remaining == 0) {
      if (ioctl(d->client_handle, FIONREAD, &n) != 0) {
        connection_reset(d);
        return;
      }
      if (n >= 1) {
        d->rx_buffer_size =
            read(d->client_handle, &d->rx_buffer, sizeof(d->rx_buffer));
        if (d->rx_buffer_size < 0) {
          connection_reset(d);
          return;
        }
        d->rx_buffer_remaining = d->rx_buffer_size;
      } else {
        d->self_sleep = 30;
        return;
      }
    }

    if (d->rx_buffer_remaining != 0) {
      char command =
          d->rx_buffer[d->rx_buffer_size - (d->rx_buffer_remaining--)];
      switch (command) {
      case 'R': { // Read request
        char tdo_value = (output->tdo != 0) ? '1' : '0';
        if (send(d->client_handle, &tdo_value, 1, 0) == -1) {
          printf("[REMOTE_BITBANG] send failed\n");
          connection_reset(d);
        }

        break;
      }
      case '0' ... '7': { // Write tck, tms, tdi
        uint8_t value = command - '0';
        input->tck = (value & 4) != 0;
        input->tms = (value & 2) != 0;
        input->tdi = (value & 1) != 0;
        d->prev_input = *input;
        break;
      }
      case 'Q': { // Quit
        printf("[REMOTE_BITBANG] Received `Q`, closing socket.\n");
        connection_reset(d);
        // Consider terminating the simulation here
        printf("[REMOTE_BITBANG] Simulation will not be terminated.\n");
        break;
      }
      case 'B': { // Blink on
        // Do not print anything, it will flood the output
        break;
      }
      case 'b': { // Blink off
        // Do not print anything, it will flood the output
        break;
      }
      case 'r': { // Reset 0 0
        printf("[REMOTE_BITBANG] Command 'r': Reset 0 0 not implemented\n");
        break;
      }
      case 's': { // Reset 0 1
        printf("[REMOTE_BITBANG] Command 's': Reset 0 1 not implemented\n");
        break;
      }
      case 't': { // Reset 1 1
        printf("[REMOTE_BITBANG] Command 't': Reset 1 1 not implemented\n");
        break;
      }
      case 'u': { // Reset 1 0
        printf("[REMOTE_BITBANG] Command 'u': Reset 1 0 not implemented\n");
        break;
      }
      default: {
        printf("[REMOTE_BITBANG] Unknown command: %c\n", command);
        connection_reset(d);
        break;
      }
      }
    }
  }

  d->timer = 3;
}

void vexr_jtag_bridge_shutdown(vexr_jtag_bridge_data *bridge_data) {
  if (bridge_data->client_handle != -1) {
    shutdown(bridge_data->client_handle, SHUT_RDWR);
    usleep(100);
  }
  if (bridge_data->server_socket != -1) {
    close(bridge_data->server_socket);
    usleep(100);
  }
}

/** Returns true on success, or false if there was an error */
static bool set_socket_blocking_enabled(int fd, bool blocking) {
  if (fd < 0)
    return false;

#ifdef WIN32
  unsigned long mode = blocking ? 0 : 1;
  return (ioctlsocket(fd, FIONBIO, &mode) == 0) ? true : false;
#else
  int flags = fcntl(fd, F_GETFL, 0);
  if (flags < 0)
    return false;
  flags = blocking ? (flags & ~O_NONBLOCK) : (flags | O_NONBLOCK);
  return (fcntl(fd, F_SETFL, flags) == 0) ? true : false;
#endif
}

static void connection_reset(vexr_jtag_bridge_data *bridge_data) {
  printf("[JTAG BRIDGE] closed connection\n");
  shutdown(bridge_data->client_handle, SHUT_RDWR);
  bridge_data->client_handle = -1;
}
