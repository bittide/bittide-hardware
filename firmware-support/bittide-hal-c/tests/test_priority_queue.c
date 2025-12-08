// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

// Include the priority queue header and implementation
#include "../c_src/priority_queue.c"
#include "priority_queue.h"

// Test counter
static int tests_passed = 0;
static int tests_failed = 0;

#define TEST(name)                                                             \
  printf("\n--- Test: " #name " ---\n");                                       \
  test_##name();                                                               \
  tests_passed++;

void test_initialization() {
  FixedIntPriorityQueue pq;
  pq_init(&pq);

  assert(pq_is_empty(&pq));
  assert(pq_size(&pq) == 0);
  assert(!pq_is_full(&pq));

  printf("✓ Queue initialized correctly\n");
}

void test_single_insert_and_extract() {
  FixedIntPriorityQueue pq;
  pq_init(&pq);

  pq_insert(&pq, 42, 42);
  assert(pq_size(&pq) == 1);
  assert(!pq_is_empty(&pq));

  PriorityQueueItem item = pq_extract_min(&pq);
  assert(item.data == 42);
  assert(item.priority == 42);
  assert(pq_is_empty(&pq));
  assert(pq_size(&pq) == 0);

  printf("✓ Single insert and extract works\n");
}

void test_peek() {
  FixedIntPriorityQueue pq;
  pq_init(&pq);

  pq_insert(&pq, 100, 100);
  PriorityQueueItem peeked = pq_peek_min(&pq);

  assert(peeked.data == 100);
  assert(peeked.priority == 100);
  assert(pq_size(&pq) == 1); // Size shouldn't change after peek

  PriorityQueueItem extracted = pq_extract_min(&pq);
  assert(extracted.data == 100);
  assert(extracted.priority == 100);
  assert(pq_is_empty(&pq));

  printf("✓ Peek doesn't remove item\n");
}

void test_priority_ordering() {
  FixedIntPriorityQueue pq;
  pq_init(&pq);

  // Insert in random order (using same value for data and priority)
  pq_insert(&pq, 50, 50);
  pq_insert(&pq, 10, 10);
  pq_insert(&pq, 30, 30);
  pq_insert(&pq, 20, 20);
  pq_insert(&pq, 40, 40);

  assert(pq_size(&pq) == 5);

  // Should extract in ascending order (lowest priority first)
  assert(pq_extract_min(&pq).priority == 10);
  assert(pq_extract_min(&pq).priority == 20);
  assert(pq_extract_min(&pq).priority == 30);
  assert(pq_extract_min(&pq).priority == 40);
  assert(pq_extract_min(&pq).priority == 50);

  assert(pq_is_empty(&pq));

  printf("✓ Items extracted in correct priority order\n");
}

void test_duplicate_values() {
  FixedIntPriorityQueue pq;
  pq_init(&pq);

  pq_insert(&pq, 25, 25);
  pq_insert(&pq, 25, 25);
  pq_insert(&pq, 25, 25);

  assert(pq_size(&pq) == 3);

  assert(pq_extract_min(&pq).priority == 25);
  assert(pq_extract_min(&pq).priority == 25);
  assert(pq_extract_min(&pq).priority == 25);

  assert(pq_is_empty(&pq));

  printf("✓ Handles duplicate values correctly\n");
}

void test_extreme_values() {
  FixedIntPriorityQueue pq;
  pq_init(&pq);

  // Test with 64-bit extremes
  uint64_t max_val = UINT64_MAX;
  uint64_t min_val = 0;

  pq_insert(&pq, max_val, max_val);
  pq_insert(&pq, min_val, min_val);
  pq_insert(&pq, 500, 500);

  assert(pq_extract_min(&pq).priority == min_val);
  assert(pq_extract_min(&pq).priority == 500);
  assert(pq_extract_min(&pq).priority == max_val);

  printf("✓ Handles extreme 64-bit values\n");
}

void test_fill_and_empty() {
  FixedIntPriorityQueue pq;
  pq_init(&pq);

  // Initially not full
  assert(!pq_is_full(&pq));

  // Fill the queue to one space left (MAX_FIXED_PQ_SIZE - 1)
  for (uint32_t i = 0; i < MAX_FIXED_PQ_SIZE - 1; i++) {
    pq_insert(&pq, i, i);
    assert(!pq_is_full(&pq)); // Should not be full yet
    assert(pq_size(&pq) == i + 1);
  }

  // Add one more item to reach capacity
  pq_insert(&pq, MAX_FIXED_PQ_SIZE - 1, MAX_FIXED_PQ_SIZE - 1);
  assert(pq_is_full(&pq)); // Now it should be full
  assert(pq_size(&pq) == MAX_FIXED_PQ_SIZE);

  // Peek should not change is_full status
  PriorityQueueItem peeked = pq_peek_min(&pq);
  assert(peeked.priority == 0); // Minimum value
  assert(pq_is_full(&pq));      // Still full after peek
  assert(pq_size(&pq) == MAX_FIXED_PQ_SIZE);

  // Extract one item - is_full should become false
  PriorityQueueItem extracted = pq_extract_min(&pq);
  assert(extracted.priority == 0);
  assert(!pq_is_full(&pq)); // No longer full
  assert(pq_size(&pq) == MAX_FIXED_PQ_SIZE - 1);

  // Empty the rest of the queue
  for (uint32_t i = 1; i < MAX_FIXED_PQ_SIZE; i++) {
    PriorityQueueItem item = pq_extract_min(&pq);
    assert(item.priority == i);
    assert(!pq_is_full(&pq)); // Should never be full while emptying
  }

  // After emptying, should be empty. We checked the full flag above
  assert(pq_is_empty(&pq));

  printf("✓ Can fill to capacity and empty completely\n");
  printf("✓ is_full flag works correctly\n");
  printf("✓ peek doesn't affect is_full flag\n");
  printf("✓ extract clears is_full flag\n");
}

void test_alternating_insert_extract() {
  FixedIntPriorityQueue pq;
  pq_init(&pq);

  // Interleave insertions and extractions
  pq_insert(&pq, 30, 30);
  pq_insert(&pq, 10, 10);
  assert(pq_extract_min(&pq).priority == 10);

  pq_insert(&pq, 20, 20);
  pq_insert(&pq, 5, 5);
  assert(pq_extract_min(&pq).priority == 5);
  assert(pq_extract_min(&pq).priority == 20);

  pq_insert(&pq, 15, 15);
  assert(pq_extract_min(&pq).priority == 15);
  assert(pq_extract_min(&pq).priority == 30);

  assert(pq_is_empty(&pq));

  printf("✓ Alternating insert/extract works correctly\n");
}

void test_reverse_order_insertion() {
  FixedIntPriorityQueue pq;
  pq_init(&pq);

  // Insert in descending order
  for (int i = 20; i > 0; i--) {
    pq_insert(&pq, i, i);
  }

  // Should still extract in ascending order
  for (int i = 1; i <= 20; i++) {
    assert(pq_extract_min(&pq).priority == (uint64_t)i);
  }

  assert(pq_is_empty(&pq));

  printf("✓ Reverse order insertion handled correctly\n");
}

void test_find_min_index() {
  FixedIntPriorityQueue pq;
  pq_init(&pq);

  pq_insert(&pq, 100, 100);
  pq_insert(&pq, 50, 50);
  pq_insert(&pq, 75, 75);

  uint32_t min_idx = find_min_index(&pq);
  assert(pq.items[min_idx].priority == 50);

  pq_extract_min(&pq); // Remove 50

  min_idx = find_min_index(&pq);
  assert(pq.items[min_idx].priority == 75);

  printf("✓ find_min_index returns correct index\n");
}

int main() {
  printf("=== Priority Queue Test Suite ===\n");
  printf("Running comprehensive tests on native system...\n");

  TEST(initialization);
  TEST(single_insert_and_extract);
  TEST(peek);
  TEST(priority_ordering);
  TEST(duplicate_values);
  TEST(extreme_values);
  TEST(fill_and_empty);
  TEST(alternating_insert_extract);
  TEST(reverse_order_insertion);
  TEST(find_min_index);

  printf("\n=== Test Summary ===\n");
  printf("Tests passed: %d\n", tests_passed);
  printf("Tests failed: %d\n", tests_failed);

  if (tests_failed == 0) {
    printf("\n✓ All tests passed!\n");
    return 0;
  } else {
    printf("\n✗ Some tests failed!\n");
    return 1;
  }
}
