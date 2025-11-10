// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include <stdio.h>
#include <assert.h>
#include <stdint.h>
#include <stdbool.h>

// Include the priority queue header and implementation
#include "priority_queue.h"
#include "../src/priority_queue.c"

// Test counter
static int tests_passed = 0;
static int tests_failed = 0;

#define TEST(name) \
    printf("\n--- Test: " #name " ---\n"); \
    test_##name(); \
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

    pq_insert(&pq, 42);
    assert(pq_size(&pq) == 1);
    assert(!pq_is_empty(&pq));

    uint64_t val = pq_extract_min(&pq);
    assert(val == 42);
    assert(pq_is_empty(&pq));
    assert(pq_size(&pq) == 0);

    printf("✓ Single insert and extract works\n");
}

void test_peek() {
    FixedIntPriorityQueue pq;
    pq_init(&pq);

    pq_insert(&pq, 100);
    uint64_t peeked = pq_peek_min(&pq);

    assert(peeked == 100);
    assert(pq_size(&pq) == 1); // Size shouldn't change after peek

    uint64_t extracted = pq_extract_min(&pq);
    assert(extracted == 100);
    assert(pq_is_empty(&pq));

    printf("✓ Peek doesn't remove item\n");
}

void test_priority_ordering() {
    FixedIntPriorityQueue pq;
    pq_init(&pq);

    // Insert in arbitrary order
    pq_insert(&pq, 50);
    pq_insert(&pq, 10);
    pq_insert(&pq, 30);
    pq_insert(&pq, 20);
    pq_insert(&pq, 40);

    assert(pq_size(&pq) == 5);

    // Should extract in ascending order (lowest value = highest priority)
    assert(pq_extract_min(&pq) == 10);
    assert(pq_extract_min(&pq) == 20);
    assert(pq_extract_min(&pq) == 30);
    assert(pq_extract_min(&pq) == 40);
    assert(pq_extract_min(&pq) == 50);

    assert(pq_is_empty(&pq));

    printf("✓ Items extracted in correct priority order\n");
}

void test_duplicate_values() {
    FixedIntPriorityQueue pq;
    pq_init(&pq);

    pq_insert(&pq, 25);
    pq_insert(&pq, 25);
    pq_insert(&pq, 25);

    assert(pq_size(&pq) == 3);

    assert(pq_extract_min(&pq) == 25);
    assert(pq_extract_min(&pq) == 25);
    assert(pq_extract_min(&pq) == 25);

    assert(pq_is_empty(&pq));

    printf("✓ Handles duplicate values correctly\n");
}

void test_extreme_values() {
    FixedIntPriorityQueue pq;
    pq_init(&pq);

    // Test with 64-bit extremes
    uint64_t max_val = UINT64_MAX;
    uint64_t min_val = 0;

    pq_insert(&pq, max_val);
    pq_insert(&pq, min_val);
    pq_insert(&pq, 500);

    assert(pq_extract_min(&pq) == min_val);
    assert(pq_extract_min(&pq) == 500);
    assert(pq_extract_min(&pq) == max_val);

    printf("✓ Handles extreme 64-bit values\n");
}

void test_fill_and_empty() {
    FixedIntPriorityQueue pq;
    pq_init(&pq);

    // Initially not full
    assert(!pq_is_full(&pq));

    // Fill the queue to one space left (MAX_FIXED_PQ_SIZE - 1)
    for (uint32_t i = 0; i < MAX_FIXED_PQ_SIZE - 1; i++) {
        pq_insert(&pq, i);
        assert(!pq_is_full(&pq)); // Should not be full yet
        assert(pq_size(&pq) == i + 1);
    }

    // Add one more item to reach capacity
    pq_insert(&pq, MAX_FIXED_PQ_SIZE - 1);
    assert(pq_is_full(&pq)); // Now it should be full
    assert(pq_size(&pq) == MAX_FIXED_PQ_SIZE);

    // Peek should not change is_full status
    uint64_t peeked = pq_peek_min(&pq);
    assert(peeked == 0); // Minimum value
    assert(pq_is_full(&pq)); // Still full after peek
    assert(pq_size(&pq) == MAX_FIXED_PQ_SIZE);

    // Extract one item - is_full should become false
    uint64_t extracted = pq_extract_min(&pq);
    assert(extracted == 0);
    assert(!pq_is_full(&pq)); // No longer full
    assert(pq_size(&pq) == MAX_FIXED_PQ_SIZE - 1);

    // Empty the rest of the queue
    for (uint32_t i = 1; i < MAX_FIXED_PQ_SIZE; i++) {
        uint64_t val = pq_extract_min(&pq);
        assert(val == i);
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
    pq_insert(&pq, 30);
    pq_insert(&pq, 10);
    assert(pq_extract_min(&pq) == 10);

    pq_insert(&pq, 20);
    pq_insert(&pq, 5);
    assert(pq_extract_min(&pq) == 5);
    assert(pq_extract_min(&pq) == 20);

    pq_insert(&pq, 15);
    assert(pq_extract_min(&pq) == 15);
    assert(pq_extract_min(&pq) == 30);

    assert(pq_is_empty(&pq));

    printf("✓ Alternating insert/extract works correctly\n");
}

void test_reverse_order_insertion() {
    FixedIntPriorityQueue pq;
    pq_init(&pq);

    // Insert in descending order
    for (int i = 20; i > 0; i--) {
        pq_insert(&pq, i);
    }

    // Should still extract in ascending order
    for (int i = 1; i <= 20; i++) {
        assert(pq_extract_min(&pq) == (uint64_t)i);
    }

    assert(pq_is_empty(&pq));

    printf("✓ Reverse order insertion handled correctly\n");
}

void test_find_min_index() {
    FixedIntPriorityQueue pq;
    pq_init(&pq);

    pq_insert(&pq, 100);
    pq_insert(&pq, 50);
    pq_insert(&pq, 75);

    uint32_t min_idx = find_min_index(&pq);
    assert(pq.items[min_idx] == 50);

    pq_extract_min(&pq); // Remove 50

    min_idx = find_min_index(&pq);
    assert(pq.items[min_idx] == 75);

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
