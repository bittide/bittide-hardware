// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "hals/soft_ugn_demo_gppe/device_instances.h"

#include "bittide_gather.h"
#include "bittide_scatter.h"
#include "bittide_timer.h"
#include "bittide_uart.h"
#include "bittide_ugn.h"
#include "messages.h"

// Prevent forward declarations since we have the real types
#define RINGBUFFER_ALIGN_H_SKIP_FORWARD_DECLS
#include "ringbuffer_align.h"

// ============================================================================
// Gather Unit Functions (TX/Outgoing)
// ============================================================================

void ringbuffer_set_alignment(GatherUnit gather,
                              enum RingbufferAlignState state) {
  uint64_t encoded_msg = (uint64_t)(state);
  gather_unit_set_gather_memory_unchecked(gather, 0,
                                          (uint8_t const *)&encoded_msg);
}

// ============================================================================
// Scatter Unit Functions (RX/Incoming)
// ============================================================================

bool ringbuffer_find_alignment(ScatterUnit scatter, int16_t buffer_size,
                               int16_t *found_offset,
                               enum RingbufferAlignState *found_state) {
  // Initialize outputs
  *found_offset = -1;
  *found_state = RINGBUFFER_ALIGN_EMPTY;

  // Scan the entire ringbuffer
  for (int16_t rx_idx = 0; rx_idx < buffer_size; rx_idx++) {
    uint64_t scatter_data;
    scatter_unit_get_scatter_memory_unchecked(scatter, rx_idx,
                                              (uint8_t *)&scatter_data);

    enum RingbufferAlignState state = (enum RingbufferAlignState)(scatter_data);

    // Check if we found an alignment message
    if (state == RINGBUFFER_ALIGN_ANNOUNCE ||
        state == RINGBUFFER_ALIGN_ACKNOWLEDGE) {
      *found_offset = rx_idx;
      *found_state = state;
      return true;
    }
  }

  return false; // No alignment message found
}

enum RingbufferAlignState
ringbuffer_get_alignment_at_offset(ScatterUnit scatter, int16_t offset) {
  uint64_t scatter_data;
  scatter_unit_get_scatter_memory_unchecked(scatter, offset,
                                            (uint8_t *)&scatter_data);
  return (enum RingbufferAlignState)(scatter_data);
}

// ============================================================================
// Ringbuffer Alignment Protocol
// ============================================================================

void align_ringbuffers(UgnContext *ugn_ctx, int16_t *incoming_offsets,
                       Uart uart) {
  PRINT_ALIGN_START(uart);

  const int32_t num_ports = ugn_ctx->num_ports;
  bool received_ack[num_ports];
  uint32_t iteration = 0;

  // Initialize arrays
  for (int32_t port = 0; port < num_ports; port++) {
    incoming_offsets[port] = -1; // -1 means not found yet
    received_ack[port] = false;
  }

  // Step 2: Initialize - Write ALIGNMENT_ANNOUNCE to TX index 0, clear rest
  for (int32_t port = 0; port < num_ports; port++) {
    GatherUnit gather = ugn_ctx->gather_units[port];

    // Clear rest of buffer
    uint64_t empty_msg = (uint64_t)(RINGBUFFER_ALIGN_EMPTY);
    for (int16_t i = 1; i < GATHER_UNIT_GATHER_MEMORY_LEN; i++) {
      gather_unit_set_gather_memory_unchecked(gather, i,
                                              (uint8_t const *)&empty_msg);
    }

    // Write ALIGNMENT_ANNOUNCE at index 0
    ringbuffer_set_alignment(gather, RINGBUFFER_ALIGN_ANNOUNCE);
  }

  // ========================================================================
  // PHASE 1: Search for incoming messages and discover offsets
  // ========================================================================
  uart_puts(uart, "Phase 1: Searching for announce messages...\n");

  bool phase1_complete = false;
  while (!phase1_complete) {
    iteration++;

    for (int32_t port = 0; port < num_ports; port++) {
      if (incoming_offsets[port] >= 0) {
        continue; // Already found offset for this port
      }

      ScatterUnit scatter = ugn_ctx->scatter_units[port];

      // Use the scan function to search for alignment messages
      int16_t found_offset;
      enum RingbufferAlignState found_state;
      if (ringbuffer_find_alignment(scatter, 4000, &found_offset,
                                    &found_state)) {
        // Found message
        if (found_state == RINGBUFFER_ALIGN_ACKNOWLEDGE) {
          received_ack[port] = true;
        }
        incoming_offsets[port] = found_offset;
        PRINT_ALIGN_STATE_CHANGE(uart, iteration, port, found_state,
                                 found_offset);
      }
    }

    // Check if Phase 1 is complete (all offsets found)
    phase1_complete = true;
    for (int32_t port = 0; port < num_ports; port++) {
      if (incoming_offsets[port] < 0) {
        phase1_complete = false;
        break;
      }
    }
  }

  uart_puts(uart, "Phase 1 complete! All offsets discovered.\n");

  // ========================================================================
  // PHASE 2: Change to ACKNOWLEDGE and wait for all partners to acknowledge
  // ========================================================================
  uart_puts(uart,
            "Phase 2: Sending acknowledges and waiting for confirmation...\n");

  // Change all outgoing messages to ACKNOWLEDGE
  for (int32_t port = 0; port < num_ports; port++) {
    GatherUnit gather = ugn_ctx->gather_units[port];
    ringbuffer_set_alignment(gather, RINGBUFFER_ALIGN_ACKNOWLEDGE);
  }

  // Wait for all partners to send ACKNOWLEDGE
  bool phase2_complete = false;
  while (!phase2_complete) {
    iteration++;

    for (int32_t port = 0; port < num_ports; port++) {
      if (received_ack[port]) {
        continue; // Already received ACK for this port
      }

      ScatterUnit scatter = ugn_ctx->scatter_units[port];

      // Check at the known position for ACKNOWLEDGE
      enum RingbufferAlignState state =
          ringbuffer_get_alignment_at_offset(scatter, incoming_offsets[port]);

      if (state == RINGBUFFER_ALIGN_ACKNOWLEDGE) {
        received_ack[port] = true;
        PRINT_ALIGN_STATE_CHANGE(uart, iteration, port, state,
                                 incoming_offsets[port]);
      }
    }

    // Check if Phase 2 is complete (all ACKs received)
    phase2_complete = true;
    for (int32_t port = 0; port < num_ports; port++) {
      if (!received_ack[port]) {
        phase2_complete = false;
        break;
      }
    }
  }

  uart_puts(uart, "Phase 2 complete! All ports acknowledged.\n");
  PRINT_ALIGN_COMPLETE(uart, incoming_offsets, num_ports);
}
