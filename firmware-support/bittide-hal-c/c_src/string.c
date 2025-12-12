// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include <stdint.h>
#include <string.h>

void *memcpy(void *dest, const void *src, size_t count) {
  if (dest == 0 || src == 0) {
    return 0;
  }

  // if pointers aren't aligned we cannot do the word-based copying
  // optimisation.
  uintptr_t dest_addr = (uintptr_t)(void *)dest;
  uintptr_t src_addr = (uintptr_t)(void *)src;

  if ((dest_addr & 0x03) != (src_addr & 0x03)) {
    // do byte copying...
    uint8_t *dest_bytes = dest;
    const uint8_t *src_bytes = src;
    for (size_t i = 0; i < count; i++) {
      dest_bytes[i] = src_bytes[i];
    }
    return dest;
  }

  size_t num_words = count / sizeof(uint32_t);
  size_t start_bytes = num_words * sizeof(uint32_t);
  size_t num_bytes = count % sizeof(uint32_t);

  uint32_t *dest_words = dest;
  const uint32_t *src_words = src;

  for (size_t i = 0; i < num_words; i++) {
    dest_words[i] = src_words[i];
  }

  // copy the remaining bytes (if any)
  switch (num_bytes) {
  case 0:
    break;
  case 1: {
    // copy a single byte
    uint8_t *dest_byte = (uint8_t *)dest + start_bytes;
    const uint8_t *src_byte = (uint8_t *)src + start_bytes;
    *dest_byte = *src_byte;
  } break;
  case 2: {
    // copy a 16-bit half-word
    uint16_t *dest_half_word = (uint16_t *)((uint8_t *)dest + start_bytes);
    const uint16_t *src_half_word = (uint16_t *)((uint8_t *)src + start_bytes);
    *dest_half_word = *src_half_word;
  } break;
  case 3: {
    // copy a 16-bit half word and one byte
    uint16_t *dest_half_word = (uint16_t *)((uint8_t *)dest + start_bytes);
    const uint16_t *src_half_word = (uint16_t *)((uint8_t *)src + start_bytes);
    uint8_t *dest_byte = (uint8_t *)dest + start_bytes + sizeof(uint16_t);
    const uint8_t *src_byte = (uint8_t *)src + start_bytes + sizeof(uint16_t);

    *dest_half_word = *src_half_word;
    *dest_byte = *src_byte;
  } break;
  }

  return dest;
}

volatile void *memcpy_volatile(volatile void *dest, const volatile void *src,
                               size_t count) {
  if (dest == 0 || src == 0) {
    return 0;
  }

  // if pointers aren't aligned we cannot do the word-based copying
  // optimisation.
  uintptr_t dest_addr = (uintptr_t)(void *)dest;
  uintptr_t src_addr = (uintptr_t)(void *)src;

  if ((dest_addr & 0x03) != (src_addr & 0x03)) {
    return memcpy_volatile_bytes(dest, src, count);
  }

  size_t num_words = count / sizeof(uint32_t);
  size_t start_bytes = num_words * sizeof(uint32_t);
  size_t num_bytes = count % sizeof(uint32_t);

  volatile uint32_t *dest_words = dest;
  const volatile uint32_t *src_words = src;

  for (size_t i = 0; i < num_words; i++) {
    dest_words[i] = src_words[i];
  }

  // copy the remaining bytes (if any)
  switch (num_bytes) {
  case 0:
    break;
  case 1: {
    // copy a single byte
    volatile uint8_t *dest_byte = (volatile uint8_t *)dest + start_bytes;
    const volatile uint8_t *src_byte = (volatile uint8_t *)src + start_bytes;
    *dest_byte = *src_byte;
  } break;
  case 2: {
    // copy a 16-bit half-word
    volatile uint16_t *dest_half_word =
        (volatile uint16_t *)((volatile uint8_t *)dest + start_bytes);
    const volatile uint16_t *src_half_word =
        (volatile uint16_t *)((volatile uint8_t *)src + start_bytes);
    *dest_half_word = *src_half_word;
  } break;
  case 3: {
    // copy a 16-bit half word and one byte
    volatile uint16_t *dest_half_word =
        (volatile uint16_t *)((volatile uint8_t *)dest + start_bytes);
    const volatile uint16_t *src_half_word =
        (volatile uint16_t *)((volatile uint8_t *)src + start_bytes);
    volatile uint8_t *dest_byte =
        (volatile uint8_t *)dest + start_bytes + sizeof(uint16_t);
    const volatile uint8_t *src_byte =
        (volatile uint8_t *)src + start_bytes + sizeof(uint16_t);

    *dest_half_word = *src_half_word;
    *dest_byte = *src_byte;
  } break;
  }

  return dest;
}

volatile void *memcpy_volatile_bytes(volatile void *dest,
                                     const volatile void *src, size_t count) {
  if (dest == 0 || src == 0) {
    return 0;
  }
  volatile uint8_t *dest_bytes = dest;
  const volatile uint8_t *src_bytes = src;

  for (size_t i = 0; i < count; i++) {
    dest_bytes[i] = src_bytes[i];
  }

  return dest;
}

int memcmp(const void *lhs, const void *rhs, size_t count) {
  const uint8_t *lhs_bytes = lhs;
  const uint8_t *rhs_bytes = rhs;

  for (size_t i = 0; i < count; i++) {
    if (lhs_bytes[i] == rhs_bytes[i]) {
      continue;
    }
    if (lhs_bytes[i] < rhs_bytes[i]) {
      return -1;
    } else {
      return 1;
    }
  }
  return 0;
}
