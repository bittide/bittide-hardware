// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "priority_queue.h"

void pq_init(FixedIntPriorityQueue *pq) { pq->size = 0; }

void pq_insert(FixedIntPriorityQueue *pq, uint64_t data, uint64_t priority) {
  pq->items[pq->size].data = data;
  pq->items[pq->size].priority = priority;
  pq->size++;
}

// Assume the queue is non-empty
uint32_t find_min_index(const FixedIntPriorityQueue *pq) {
  uint64_t min_priority = pq->items[0].priority;
  uint32_t min_index = 0;
  for (uint32_t i = 1; i < pq->size; i++) {
    if (pq->items[i].priority < min_priority) {
      min_priority = pq->items[i].priority;
      min_index = i;
    }
  }
  return min_index;
}

// Extracts the item with the lowest priority value (highest priority)
// Searches the entire array, removes the item.
// Assumes queue is non-empty
//    pq: A pointer to the FixedIntPriorityQueue.
// Returns the extracted item (both data and priority).
PriorityQueueItem pq_extract_min(FixedIntPriorityQueue *pq) {
  uint32_t min_index = find_min_index(pq);
  PriorityQueueItem item_out = pq->items[min_index];
  // Replace the extracted item with the last item in the array
  pq->items[min_index] = pq->items[pq->size - 1];
  pq->size--; // Decrement size
  return item_out;
}

bool pq_is_empty(const FixedIntPriorityQueue *pq) { return pq->size == 0; }

bool pq_is_full(const FixedIntPriorityQueue *pq) {
  return pq->size >= MAX_FIXED_PQ_SIZE;
}

uint32_t pq_size(const FixedIntPriorityQueue *pq) { return pq->size; }

// Returns the item with the highest priority without removing it.
// Searches the entire array.
//        pq: A pointer to the FixedIntPriorityQueue.
// Returns the peeked item (both data and priority).
// Assumes queue is not empty.
PriorityQueueItem pq_peek_min(const FixedIntPriorityQueue *pq) {
  uint32_t min_index = find_min_index(pq);
  return pq->items[min_index];
}
