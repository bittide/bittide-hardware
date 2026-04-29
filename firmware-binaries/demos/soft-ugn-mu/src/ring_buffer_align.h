// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef RING_BUFFER_ALIGN_H
#define RING_BUFFER_ALIGN_H

// NOTE: This header requires the HAL device instance headers to be included
// before it, typically by including
// "hals/soft_ugn_demo_mu/device_instances.h" followed by the bittide_*
// headers.

#include "messages.h"
#include <stdbool.h>
#include <stdint.h>

// ============================================================================
// RingBuffer Alignment Protocol
// ============================================================================

/**
 * @brief Align ring_buffers across all ports using a two-phase protocol.
 *
 * This function implements a distributed alignment protocol that physically
 * aligns the receive ring_buffers on each port with the partner's transmit
 * stream. After alignment, alignment markers always arrive at offset 0 in
 * the receive ring_buffer.
 *
 * Per-port state machine:
 * - FINDING: Scan RX for ANNOUNCE/ACKNOWLEDGE.
 *     * If found at offset 0: write ACKNOWLEDGE to TX[0], advance.
 *     * If found at offset N > 0: call `set_clear_at_count(N)` to physically
 *       realign the hardware buffer so the marker ends up at offset 0 on
 *       subsequent iterations.
 * - ACKNOWLEDGING: Wait for partner's ACKNOWLEDGE at RX[0], then mark done.
 *
 * The caller must ensure both TX and RX ring_buffers are enabled before
 * calling this function; enables are not touched by the protocol.
 *
 * @param ugn_ctx The UGN context containing receive and transmit ring_buffers
 * @param alignment_offsets Output array (one per port) recording the offset
 *        passed to set_clear_at_count for each port. These offsets represent
 *        the counter shift applied during alignment and must be used to
 *        correct UGN receive delay measurements.
 * @param uart UART for debug output
 */
void align_ring_buffers(UgnContext *ugn_ctx, int16_t *alignment_offsets,
                        Uart uart);

#endif // RING_BUFFER_ALIGN_H
