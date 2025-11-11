// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#ifndef BITTIDE_PRIORITY_QUEUE_H
#define BITTIDE_PRIORITY_QUEUE_H

#include <stdbool.h>
#include <stdint.h>

// Maximum size of the priority queue
#ifndef MAX_FIXED_PQ_SIZE
#define MAX_FIXED_PQ_SIZE 100
#endif

// Structure to represent the priority queue
// Lower value = Higher priority.
typedef struct {
    uint64_t items[MAX_FIXED_PQ_SIZE];  // Array to store the integer items
    uint32_t size;                      // Current number of items in the array
} FixedIntPriorityQueue;

// Initializes the priority queue to empty state
void pq_init(FixedIntPriorityQueue* pq);

// Inserts an integer item into the priority queue (appends to the end).
//    pq:    A pointer to the FixedIntPriorityQueue.
//    item:  The integer item to store (acts as both data and priority).
void pq_insert(FixedIntPriorityQueue* pq, uint64_t item);

// Finds the index of the item with minimum value
// Assumes the queue is non-empty
uint32_t find_min_index(const FixedIntPriorityQueue* pq);

// Extracts the integer item with the lowest priority value
// Searches the entire array, removes the item.
// Assumes queue is non-empty
//    pq: A pointer to the FixedIntPriorityQueue.
// Returns the extracted integer.
uint64_t pq_extract_min(FixedIntPriorityQueue* pq);

// Checks if the priority queue is empty
bool pq_is_empty(const FixedIntPriorityQueue* pq);

// Checks if the priority queue is full
bool pq_is_full(const FixedIntPriorityQueue* pq);

// Returns the current size of the priority queue
uint32_t pq_size(const FixedIntPriorityQueue* pq);

// Returns the integer item with the highest priority without removing it.
// Searches the entire array.
//        pq: A pointer to the FixedIntPriorityQueue.
// Returns the peeked integer item.
// Assumes queue is not empty.
uint64_t pq_peek_min(const FixedIntPriorityQueue* pq);

#endif // BITTIDE_PRIORITY_QUEUE_H
