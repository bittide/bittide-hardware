// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_PRIORITY_QUEUE_H
#define BITTIDE_PRIORITY_QUEUE_H

#include <stdbool.h>
#include <stdint.h>

// Maximum size of the priority queue
#ifndef MAX_FIXED_PQ_SIZE
#define MAX_FIXED_PQ_SIZE 256
#endif

// Priority queue item storing both data and priority
typedef struct {
  uint64_t data;     // The data payload
  uint64_t priority; // The priority value (lower = higher priority)
} PriorityQueueItem;

// Structure to represent the priority queue
// Lower priority value = Higher priority.
typedef struct {
  PriorityQueueItem
      items[MAX_FIXED_PQ_SIZE]; // Array to store items with priorities
  uint32_t size;                // Current number of items in the array
} FixedIntPriorityQueue;

// Initializes the priority queue to empty state
void pq_init(FixedIntPriorityQueue *pq);

// Inserts an item into the priority queue with specified priority
//    pq:       A pointer to the FixedIntPriorityQueue.
//    data:     The data payload to store
//    priority: The priority value (lower = higher priority)
void pq_insert(FixedIntPriorityQueue *pq, uint64_t data, uint64_t priority);

// Finds the index of the item with minimum value
// Assumes the queue is non-empty
uint32_t find_min_index(const FixedIntPriorityQueue *pq);

// Extracts the item with the lowest priority value (highest priority)
// Searches the entire array, removes the item.
// Assumes queue is non-empty
//    pq: A pointer to the FixedIntPriorityQueue.
// Returns the extracted item (both data and priority).
PriorityQueueItem pq_extract_min(FixedIntPriorityQueue *pq);

// Checks if the priority queue is empty
bool pq_is_empty(const FixedIntPriorityQueue *pq);

// Checks if the priority queue is full
bool pq_is_full(const FixedIntPriorityQueue *pq);

// Returns the current size of the priority queue
uint32_t pq_size(const FixedIntPriorityQueue *pq);

// Returns the item with the highest priority without removing it.
// Searches the entire array.
//        pq: A pointer to the FixedIntPriorityQueue.
// Returns the peeked item (both data and priority).
// Assumes queue is not empty.
PriorityQueueItem pq_peek_min(const FixedIntPriorityQueue *pq);

#endif // BITTIDE_PRIORITY_QUEUE_H
