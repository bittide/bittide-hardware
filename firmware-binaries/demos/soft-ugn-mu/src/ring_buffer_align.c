// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "hals/soft_ugn_demo_mu/device_instances.h"

#include "bittide_ring_receive.h"
#include "bittide_ring_transmit.h"
#include "bittide_timer.h"
#include "bittide_uart.h"
#include "bittide_ugn.h"
#include "messages.h"

#include "ring_buffer_align.h"

// ============================================================================
// RingBuffer Alignment Protocol
// ============================================================================

enum AlignPhase {
  ALIGN_PHASE_FINDING,
  ALIGN_PHASE_ACKNOWLEDGING,
  ALIGN_PHASE_DONE,
};

static void write_marker(TransmitRingBuffer tx_ring, int16_t idx,
                         enum RingBufferAlignState state) {
  uint64_t encoded = (uint64_t)state;
  transmit_ring_buffer_set_data_unchecked(tx_ring, idx,
                                          (uint8_t const *)&encoded);
}

static enum RingBufferAlignState read_state(ReceiveRingBuffer rx_ring,
                                            int16_t idx) {
  uint64_t data;
  receive_ring_buffer_get_data_unchecked(rx_ring, idx, (uint8_t *)&data);
  return (enum RingBufferAlignState)data;
}

void align_ring_buffers(UgnContext *ugn_ctx, Uart uart) {
  PRINT_ALIGN_START(uart);

  const int32_t num_ports = ugn_ctx->num_ports;
  enum AlignPhase phases[num_ports];

  // Initialize: clear each TX buffer and announce at TX[0].
  for (int32_t port = 0; port < num_ports; port++) {
    TransmitRingBuffer tx_ring = ugn_ctx->transmit_ring_buffers[port];
    for (int16_t i = 0; i < TRANSMIT_RING_BUFFER_DATA_LEN; i++) {
      write_marker(tx_ring, i, RING_BUFFER_ALIGN_EMPTY);
    }
    write_marker(tx_ring, 0, RING_BUFFER_ALIGN_ANNOUNCE);
    phases[port] = ALIGN_PHASE_FINDING;
  }

  uint32_t iteration = 0;
  bool all_done = false;
  while (!all_done) {
    iteration++;
    all_done = true;

    for (int32_t port = 0; port < num_ports; port++) {
      if (phases[port] == ALIGN_PHASE_DONE) {
        continue;
      }
      all_done = false;

      ReceiveRingBuffer rx_ring = ugn_ctx->receive_ring_buffers[port];
      TransmitRingBuffer tx_ring = ugn_ctx->transmit_ring_buffers[port];

      if (phases[port] == ALIGN_PHASE_FINDING) {
        // Scan the whole RX buffer for an alignment marker. If it lands at
        // offset 0 we can acknowledge directly; otherwise we tell the
        // hardware to realign by resetting its counter at that offset.
        for (int16_t rx_idx = 0; rx_idx < RECEIVE_RING_BUFFER_DATA_LEN;
             rx_idx++) {
          enum RingBufferAlignState state = read_state(rx_ring, rx_idx);
          if (state != RING_BUFFER_ALIGN_ANNOUNCE &&
              state != RING_BUFFER_ALIGN_ACKNOWLEDGE) {
            continue;
          }

          PRINT_ALIGN_STATE_CHANGE(uart, iteration, port, state, rx_idx);
          if (rx_idx == 0) {
            write_marker(tx_ring, 0, RING_BUFFER_ALIGN_ACKNOWLEDGE);
            phases[port] = ALIGN_PHASE_ACKNOWLEDGING;
            break;
          } else {
            receive_ring_buffer_set_clear_at_count(rx_ring, rx_idx);
          }
        }
      } else if (phases[port] == ALIGN_PHASE_ACKNOWLEDGING ||
                 phases[port] == 0) {
        // Partner's ACKNOWLEDGE arrives at offset 0 once the hardware is
        // aligned and they have also transitioned past FINDING.
        enum RingBufferAlignState state = read_state(rx_ring, 0);
        if (state == RING_BUFFER_ALIGN_ACKNOWLEDGE ||
            state == RING_BUFFER_ALIGN_EMPTY) {
          PRINT_ALIGN_STATE_CHANGE(uart, iteration, port, state, 0);
          phases[port] = ALIGN_PHASE_DONE;
        }
      }
    }
  }

  uart_puts(uart, "RingBuffer alignment complete!\n");
}
