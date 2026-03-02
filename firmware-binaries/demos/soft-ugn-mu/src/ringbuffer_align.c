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

// Prevent forward declarations since we have the real types
#define RINGBUFFER_ALIGN_H_SKIP_FORWARD_DECLS
#include "ringbuffer_align.h"

// ============================================================================
// Transmit Ringbuffer Functions (TX/Outgoing)
// ============================================================================

void ringbuffer_set_alignment(TransmitRingbuffer tx_ring,
                              enum RingbufferAlignState state) {
  uint64_t encoded_msg = (uint64_t)(state);
  transmit_ringbuffer_set_data_unchecked(tx_ring, 0,
                                         (uint8_t const *)&encoded_msg);
}

// ============================================================================
// Receive Ringbuffer Functions (RX/Incoming)
// ============================================================================

bool ringbuffer_find_alignment(ReceiveRingbuffer rx_ring, int16_t buffer_size,
                               int16_t *found_offset,
                               enum RingbufferAlignState *found_state) {
  // Initialize outputs
  *found_offset = -1;
  *found_state = RINGBUFFER_ALIGN_EMPTY;

  // Scan the entire ringbuffer
  for (int16_t rx_idx = 0; rx_idx < buffer_size; rx_idx++) {
    uint64_t rx_data;
    receive_ringbuffer_get_data_unchecked(rx_ring, rx_idx, (uint8_t *)&rx_data);

    enum RingbufferAlignState state = (enum RingbufferAlignState)(rx_data);

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
ringbuffer_get_alignment_at_offset(ReceiveRingbuffer rx_ring, int16_t offset) {
  uint64_t rx_data;
  receive_ringbuffer_get_data_unchecked(rx_ring, offset, (uint8_t *)&rx_data);
  return (enum RingbufferAlignState)(rx_data);
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
    TransmitRingbuffer tx_ring = ugn_ctx->transmit_ringbuffers[port];

    // Clear rest of buffer
    uint64_t empty_msg = (uint64_t)(RINGBUFFER_ALIGN_EMPTY);
    for (int16_t i = 1; i < TRANSMIT_RINGBUFFER_DATA_LEN; i++) {
      transmit_ringbuffer_set_data_unchecked(tx_ring, i,
                                             (uint8_t const *)&empty_msg);
    }

    // Write ALIGNMENT_ANNOUNCE at index 0
    ringbuffer_set_alignment(tx_ring, RINGBUFFER_ALIGN_ANNOUNCE);
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

      ReceiveRingbuffer rx_ring = ugn_ctx->receive_ringbuffers[port];

      // Use the scan function to search for alignment messages
      int16_t found_offset;
      enum RingbufferAlignState found_state;
      if (ringbuffer_find_alignment(rx_ring, 4000, &found_offset,
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
    TransmitRingbuffer tx_ring = ugn_ctx->transmit_ringbuffers[port];
    ringbuffer_set_alignment(tx_ring, RINGBUFFER_ALIGN_ACKNOWLEDGE);
  }

  // Wait for all partners to send ACKNOWLEDGE
  bool phase2_complete = false;
  while (!phase2_complete) {
    iteration++;

    for (int32_t port = 0; port < num_ports; port++) {
      if (received_ack[port]) {
        continue; // Already received ACK for this port
      }

      ReceiveRingbuffer rx_ring = ugn_ctx->receive_ringbuffers[port];

      // Check at the known position for ACKNOWLEDGE
      enum RingbufferAlignState state =
          ringbuffer_get_alignment_at_offset(rx_ring, incoming_offsets[port]);

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
