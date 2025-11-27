#!/bin/bash
# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

# This script compiles and runs C-based software tests that can be run in a standard
# Linux environment instead of on embedded hardware.
set -e

# Create target directory if it doesn't exist
mkdir -p target

# Compiler settings
CC=gcc
CFLAGS="-std=c99 -Wall -Wextra"
INCLUDE_DIRS="-I include -I src"

# Array of test files (add new tests here)
TEST_FILES=(
    "test_priority_queue"
)

# Track overall results
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

echo "=== Building and Running C Test Suite ==="
echo ""

# Compile and run each test
for test in "${TEST_FILES[@]}"; do
    TOTAL_TESTS=$((TOTAL_TESTS + 1))

    echo "[$TOTAL_TESTS] Compiling $test..."

    if $CC $CFLAGS $INCLUDE_DIRS -o "target/$test" "tests/$test.c"; then
        echo "[$TOTAL_TESTS] Running $test..."

        if "./target/$test"; then
            echo "✓ $test passed"
            PASSED_TESTS=$((PASSED_TESTS + 1))
        else
            echo "✗ $test failed"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    else
        echo "✗ $test compilation failed"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi

    echo ""
done

# Print summary
echo "=== Test Summary ==="
echo "Total: $TOTAL_TESTS"
echo "Passed: $PASSED_TESTS"
echo "Failed: $FAILED_TESTS"
echo ""

if [ $FAILED_TESTS -eq 0 ]; then
    echo "✓ All tests passed!"
    exit 0
else
    echo "✗ Some tests failed!"
    exit 1
fi
