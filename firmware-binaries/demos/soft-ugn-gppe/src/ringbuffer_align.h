// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef RINGBUFFER_ALIGN_H
#define RINGBUFFER_ALIGN_H

// NOTE: This header requires the HAL device instance headers to be included
// before it, typically by including
// "hals/soft_ugn_demo_gppe/device_instances.h" followed by the bittide_*
// headers.

#include "messages.h"
#include <stdbool.h>
#include <stdint.h>

// ============================================================================
// Gather Unit Functions (TX/Outgoing)
// ============================================================================

/**
 * @brief Set a gather unit's first address to a specific alignment state.
 *
 * This writes the encoded alignment state value to index 0 of the gather unit's
 * memory, which is the first address that will be transmitted.
 *
 * @param gather The gather unit to configure
 * @param state The alignment state to write at the first address
 */
void ringbuffer_set_alignment(GatherUnit gather,
                              enum RingbufferAlignState state);

// ============================================================================
// Scatter Unit Functions (RX/Incoming)
// ============================================================================

/**
 * @brief Scan a scatter unit's ringbuffer for alignment state messages.
 *
 * This function scans the entire scatter unit memory looking for
 * RINGBUFFER_ALIGN_ANNOUNCE or RINGBUFFER_ALIGN_ACKNOWLEDGE messages.
 *
 * @param scatter The scatter unit to scan
 * @param buffer_size The size of the ringbuffer to scan
 * @param found_offset Pointer to store the offset where a message was found
 *                     (set to -1 if not found)
 * @param found_state Pointer to store the state that was found
 *                    (only valid if found_offset >= 0)
 * @return bool True if an alignment message was found, false otherwise
 */
bool ringbuffer_find_alignment(ScatterUnit scatter, int16_t buffer_size,
                               int16_t *found_offset,
                               enum RingbufferAlignState *found_state);

/**
 * @brief Read the alignment state at a specific offset in a scatter unit.
 *
 * @param scatter The scatter unit to read from
 * @param offset The offset to read from
 * @return enum RingbufferAlignState The alignment state found at that offset
 */
enum RingbufferAlignState
ringbuffer_get_alignment_at_offset(ScatterUnit scatter, int16_t offset);

// ============================================================================
// Ringbuffer Alignment Protocol
// ============================================================================

/**
 * @brief Align ringbuffers across all ports using a two-phase protocol.
 *
 * This function implements a distributed alignment protocol that discovers
 * the offset of incoming messages on each port's scatter unit.
 *
 * Phase 1: Discovery
 * - Each node writes RINGBUFFER_ALIGN_ANNOUNCE to TX index 0
 * - Each node scans its RX buffers to find where partner messages arrive
 * - Stores discovered offsets in incoming_offsets array
 *
 * Phase 2: Acknowledgement
 * - Each node changes TX message to RINGBUFFER_ALIGN_ACKNOWLEDGE
 * - Each node waits for all partners to send ACKNOWLEDGE at known positions
 *
 * @param ugn_ctx The UGN context containing scatter and gather units
 * @param incoming_offsets Array to store discovered offsets for each port
 * @param uart UART for debug output
 */
void align_ringbuffers(UgnContext *ugn_ctx, int16_t *incoming_offsets,
                       Uart uart);

#endif // RINGBUFFER_ALIGN_H
