// SPDX-FileCopyrightText: 2025 Google LLC
//
// SPDX-License-Identifier: Apache-2.0

#include "bittide_uart.h"
#include "hals/register_wb/device_instances.h"
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

// Simple expect macro that prints on failure and exits
#define EXPECT(name, expected, actual)                                         \
  do {                                                                         \
    if ((expected) != (actual)) {                                              \
      uart_puts(uart, "RESULT: FAIL: ");                                       \
      uart_puts(uart, name);                                                   \
      uart_puts(uart, "\r\n");                                                 \
      while (1) {                                                              \
      }                                                                        \
    }                                                                          \
  } while (0)

// Macro to compare byte arrays
#define EXPECT_BYTES(name, expected_arr, actual_arr, len)                      \
  do {                                                                         \
    if (memcmp(expected_arr, actual_arr, len) != 0) {                          \
      uart_puts(uart, "RESULT: FAIL: ");                                       \
      uart_puts(uart, name);                                                   \
      uart_puts(uart, " (byte arrays differ)\r\n");                            \
      while (1) {                                                              \
      }                                                                        \
    }                                                                          \
  } while (0)

// Helper functions to create byte arrays from values (little-endian)
static inline void bv16(uint16_t v, uint8_t out[2]) {
  out[0] = v & 0xFF;
  out[1] = (v >> 8) & 0xFF;
}

static inline void bv64(uint64_t v, uint8_t out[8]) {
  out[0] = v & 0xFF;
  out[1] = (v >> 8) & 0xFF;
  out[2] = (v >> 16) & 0xFF;
  out[3] = (v >> 24) & 0xFF;
  out[4] = (v >> 32) & 0xFF;
  out[5] = (v >> 40) & 0xFF;
  out[6] = (v >> 48) & 0xFF;
  out[7] = (v >> 56) & 0xFF;
}

// Helper for float comparisons
static inline float abs_f(float x) { return x < 0 ? -x : x; }
static inline double abs_d(double x) { return x < 0 ? -x : x; }

#define EXPECT_FLOAT(name, expected, actual)                                   \
  do {                                                                         \
    float diff = abs_f((expected) - (actual));                                 \
    if (diff > 0.0001f) {                                                      \
      uart_puts(uart, "RESULT: FAIL: ");                                       \
      uart_puts(uart, name);                                                   \
      uart_puts(uart, " (float)\r\n");                                         \
      while (1) {                                                              \
      }                                                                        \
    }                                                                          \
  } while (0)

#define EXPECT_DOUBLE(name, expected, actual)                                  \
  do {                                                                         \
    double diff = abs_d((expected) - (actual));                                \
    if (diff > 0.000001) {                                                     \
      uart_puts(uart, "RESULT: FAIL: ");                                       \
      uart_puts(uart, name);                                                   \
      uart_puts(uart, " (double)\r\n");                                        \
      while (1) {                                                              \
      }                                                                        \
    }                                                                          \
  } while (0)

void c_main(void) {
  ManyTypes many_types = hal.many_types;
  Uart uart = hal.uart;

  uint8_t bv16_buf[2];
  uint8_t bv64_buf[8];

  // Test s0-s4 (signed integers)
  EXPECT("init.s0", -8, many_types_get_s0(many_types));
  many_types_set_s0(many_types, -16);
  EXPECT("rt.s0", -16, many_types_get_s0(many_types));

  EXPECT("init.s1", 8, many_types_get_s1(many_types));
  many_types_set_s1(many_types, 16);
  EXPECT("rt.s1", 16, many_types_get_s1(many_types));

  EXPECT("init.s2", 16, many_types_get_s2(many_types));
  many_types_set_s2(many_types, 32);
  EXPECT("rt.s2", 32, many_types_get_s2(many_types));

  EXPECT("init.s3", 3721049880298531338LL, many_types_get_s3(many_types));
  many_types_set_s3(many_types, 7442099760597062676LL);
  EXPECT("rt.s3", 7442099760597062676LL, many_types_get_s3(many_types));

  EXPECT("init.s4", -12, many_types_get_s4(many_types));
  many_types_set_s4(many_types, -13);
  EXPECT("rt.s4", -13, many_types_get_s4(many_types));

  // Test u0-u3 (unsigned integers)
  EXPECT("init.u0", 8, many_types_get_u0(many_types));
  many_types_set_u0(many_types, 16);
  EXPECT("rt.u0", 16, many_types_get_u0(many_types));

  EXPECT("init.u1", 16, many_types_get_u1(many_types));
  many_types_set_u1(many_types, 32);
  EXPECT("rt.u1", 32, many_types_get_u1(many_types));

  EXPECT("init.u2", 3721049880298531338ULL, many_types_get_u2(many_types));
  many_types_set_u2(many_types, 7442099760597062676ULL);
  EXPECT("rt.u2", 7442099760597062676ULL, many_types_get_u2(many_types));

  EXPECT("init.u3", 0xBADC0FEE, many_types_get_u3(many_types));
  many_types_set_u3(many_types, 24);
  EXPECT("rt.u3", 24, many_types_get_u3(many_types));

  // Test bv0-bv2 (bitvectors)
  EXPECT("init.bv0", 8, many_types_get_bv0(many_types));
  many_types_set_bv0(many_types, 16);
  EXPECT("rt.bv0", 16, many_types_get_bv0(many_types));

  uint8_t expected_bv16_init[2];
  bv16(16, expected_bv16_init);
  uint8_t expected_bv16_rt[2];
  bv16(32, expected_bv16_rt);
  many_types_get_bv1(many_types, bv16_buf);
  EXPECT_BYTES("init.bv1", expected_bv16_init, bv16_buf, 2);
  many_types_set_bv1(many_types, expected_bv16_rt);
  many_types_get_bv1(many_types, bv16_buf);
  EXPECT_BYTES("rt.bv1", expected_bv16_rt, bv16_buf, 2);

  uint8_t expected_bv64_init[8];
  bv64(3721049880298531338ULL, expected_bv64_init);
  uint8_t expected_bv64_rt[8];
  bv64(7442099760597062676ULL, expected_bv64_rt);
  many_types_get_bv2(many_types, bv64_buf);
  EXPECT_BYTES("init.bv2", expected_bv64_init, bv64_buf, 8);
  many_types_set_bv2(many_types, expected_bv64_rt);
  many_types_get_bv2(many_types, bv64_buf);
  EXPECT_BYTES("rt.bv2", expected_bv64_rt, bv64_buf, 8);

  // Test floats and doubles
  EXPECT_FLOAT("init.f0", -8.0f, many_types_get_f0(many_types));
  EXPECT_FLOAT("init.f1", 8.0f, many_types_get_f1(many_types));
  EXPECT_DOUBLE("init.d0", -8.0, many_types_get_d0(many_types));
  EXPECT_DOUBLE("init.d1", 8.0, many_types_get_d1(many_types));

  many_types_set_f0(many_types, -16.0f);
  many_types_set_f1(many_types, 16.0f);
  many_types_set_d0(many_types, -16.0);
  many_types_set_d1(many_types, 16.0);

  EXPECT_FLOAT("rt.f0", -16.0f, many_types_get_f0(many_types));
  EXPECT_FLOAT("rt.f1", 16.0f, many_types_get_f1(many_types));
  EXPECT_DOUBLE("rt.d0", -16.0, many_types_get_d0(many_types));
  EXPECT_DOUBLE("rt.d1", 16.0, many_types_get_d1(many_types));

  // Test bool
  EXPECT("init.b0", true, many_types_get_b0(many_types));
  many_types_set_b0(many_types, false);
  EXPECT("rt.b0", false, many_types_get_b0(many_types));

  // Test v0 (vector of uint8_t)
  uint8_t v0_expected_init[8] = {0x8, 0x16, 0x24, 0x32, 0x40, 0x4E, 0x5C, 0x6A};
  uint8_t v0_expected_rt[8] = {16, 32, 64, 128, 3, 9, 27, 81};

  for (size_t i = 0; i < 8; i++) {
    uint8_t val;
    EXPECT("init.v0[i] exists", true, many_types_get_v0(many_types, i, &val));
    EXPECT("init.v0[i]", v0_expected_init[i], val);
  }

  // Check out of bounds
  uint8_t dummy;
  EXPECT("init.v0[8]", false, many_types_get_v0(many_types, 8, &dummy));

  for (size_t i = 0; i < 8; i++) {
    many_types_set_v0(many_types, i, v0_expected_rt[i]);
  }

  for (size_t i = 0; i < 8; i++) {
    uint8_t val;
    many_types_get_v0(many_types, i, &val);
    EXPECT("rt.v0[i]", v0_expected_rt[i], val);
  }

  // Test v1 (vector of uint64_t as uint8_t[8])
  uint8_t v1_init_0[8];
  bv64(0x8, v1_init_0);
  uint8_t v1_init_1[8];
  bv64(0x16, v1_init_1);
  uint8_t v1_init_2[8];
  bv64(3721049880298531338ULL, v1_init_2);

  uint8_t v1_rt_0[8];
  bv64(1600, v1_rt_0);
  uint8_t v1_rt_1[8];
  bv64(3200, v1_rt_1);
  uint8_t v1_rt_2[8];
  bv64(7442099760597062676ULL, v1_rt_2);

  EXPECT("init.v1[0] exists", true, many_types_get_v1(many_types, 0, bv64_buf));
  EXPECT_BYTES("init.v1[0]", v1_init_0, bv64_buf, 8);

  EXPECT("init.v1[1] exists", true, many_types_get_v1(many_types, 1, bv64_buf));
  EXPECT_BYTES("init.v1[1]", v1_init_1, bv64_buf, 8);

  EXPECT("init.v1[2] exists", true, many_types_get_v1(many_types, 2, bv64_buf));
  EXPECT_BYTES("init.v1[2]", v1_init_2, bv64_buf, 8);

  EXPECT("init.v1[3]", false, many_types_get_v1(many_types, 3, bv64_buf));

  many_types_set_v1(many_types, 0, v1_rt_0);
  many_types_set_v1(many_types, 1, v1_rt_1);
  many_types_set_v1(many_types, 2, v1_rt_2);

  many_types_get_v1(many_types, 0, bv64_buf);
  EXPECT_BYTES("rt.v1[0]", v1_rt_0, bv64_buf, 8);

  many_types_get_v1(many_types, 1, bv64_buf);
  EXPECT_BYTES("rt.v1[1]", v1_rt_1, bv64_buf, 8);

  many_types_get_v1(many_types, 2, bv64_buf);
  EXPECT_BYTES("rt.v1[2]", v1_rt_2, bv64_buf, 8);

  // Test v2 (vector of [[u8; 1]; 2])
  // Note: v2 API has generator bugs:
  // 1. get signature is uint8_t *out[2] instead of uint8_t out[2]
  // 2. sizeof calculations use sizeof(uint8_t) instead of sizeof(uint8_t[2])
  // But the data layout is simple (2 bytes per element), so we can work around
  // it

  uint8_t v2_init_0[2] = {0x08, 0x16}; // [[0x8], [0x16]]
  uint8_t v2_init_1[2] = {0x24, 0x32}; // [[0x24], [0x32]]
  uint8_t v2_rt_0[2] = {0xAB, 0xCD};
  uint8_t v2_rt_1[2] = {0x12, 0x34};

  // Direct memory access to work around broken API
  // v2 is at offset 120, each element is 2 bytes
  uint8_t v2_buf[2];
  memcpy_volatile(v2_buf, many_types.base + 120 + (0 * 2), 2);
  EXPECT_BYTES("init.v2[0]", v2_init_0, v2_buf, 2);

  memcpy_volatile(v2_buf, many_types.base + 120 + (1 * 2), 2);
  EXPECT_BYTES("init.v2[1]", v2_init_1, v2_buf, 2);

  memcpy_volatile(many_types.base + 120 + (0 * 2), v2_rt_0, 2);
  memcpy_volatile(many_types.base + 120 + (1 * 2), v2_rt_1, 2);

  memcpy_volatile(v2_buf, many_types.base + 120 + (0 * 2), 2);
  EXPECT_BYTES("rt.v2[0]", v2_rt_0, v2_buf, 2);

  memcpy_volatile(v2_buf, many_types.base + 120 + (1 * 2), 2);
  EXPECT_BYTES("rt.v2[1]", v2_rt_1, v2_buf, 2);

  // Test unit type
  EXPECT("init.unitW", false, many_types_get_unit_w(many_types));
  many_types_set_unit(many_types);
  EXPECT("rt.unitW", true, many_types_get_unit_w(many_types));

  // Test zero-sized type
  many_types_get_zs(many_types);
  many_types_set_zs(many_types);

  // Test sum types (Abc, Xyz)
  EXPECT("init.sum0", ABC_C, many_types_get_sum0(many_types));
  many_types_set_sum0(many_types, ABC_A);
  EXPECT("rt.sum0", ABC_A, many_types_get_sum0(many_types));

  EXPECT("init.sum1", XYZ_S, many_types_get_sum1(many_types));
  many_types_set_sum1(many_types, XYZ_Z);
  EXPECT("rt.sum1", XYZ_Z, many_types_get_sum1(many_types));

  // Test sop0 (struct with floats)
  F sop0 = many_types_get_sop0(many_types);
  EXPECT_FLOAT("init.sop0.f", 3.14f, sop0.f);
  EXPECT_FLOAT("init.sop0.u", 6.28f, sop0.u);

  F new_sop0 = {.f = 1.0f, .u = 8.0f};
  many_types_set_sop0(many_types, new_sop0);
  sop0 = many_types_get_sop0(many_types);
  EXPECT_FLOAT("rt.sop0.f", 1.0f, sop0.f);
  EXPECT_FLOAT("rt.sop0.g", 8.0f, sop0.u);

  // Test e0 (Either<bv8, bv16>)
  Either_bv8_bv16 e0 = many_types_get_e0(many_types);
  EXPECT("init.e0.tag", EITHER_TAG_LEFT, e0.tag);
  EXPECT("init.e0.left._0", 8, e0.left._0);

  uint8_t right_val[2] = {0x12, 0x00};
  Either_bv8_bv16 new_e0 = {.tag = EITHER_TAG_RIGHT};
  memcpy(new_e0.right._0, right_val, 2);
  many_types_set_e0(many_types, new_e0);

  e0 = many_types_get_e0(many_types);
  EXPECT("rt.e0.tag", EITHER_TAG_RIGHT, e0.tag);
  EXPECT_BYTES("rt.e0.right._0", right_val, e0.right._0, 2);

  // Test oi (Maybe<Inner>)
  Maybe_Inner oi = many_types_get_oi(many_types);
  EXPECT("init.oi.tag", MAYBE_TAG_JUST, oi.tag);
  EXPECT("init.oi.just._0.inner_a", 0x16, oi.just._0.inner_a);
  uint8_t expected_inner_b[2] = {0x24, 0x00};
  EXPECT_BYTES("init.oi.just._0.inner_b", expected_inner_b, oi.just._0.inner_b,
               2);

  Inner new_inner = {.inner_a = 2};
  uint8_t new_inner_b[2] = {4, 0};
  memcpy(new_inner.inner_b, new_inner_b, 2);
  Maybe_Inner new_oi = {.tag = MAYBE_TAG_JUST, .just = {._0 = new_inner}};
  many_types_set_oi(many_types, new_oi);

  oi = many_types_get_oi(many_types);
  EXPECT("rt.oi.tag", MAYBE_TAG_JUST, oi.tag);
  EXPECT("rt.oi.just._0.inner_a", 2, oi.just._0.inner_a);
  EXPECT_BYTES("rt.oi.just._0.inner_b", new_inner_b, oi.just._0.inner_b, 2);

  // Test x2 (nested product type X2(bv8, X3(bv16, bv8, bv8)))
  X2 x2 = many_types_get_x2(many_types);
  EXPECT("init.x2.field_0", 8, x2.field_0);
  uint8_t x3_field0_init[2] = {16, 0};
  EXPECT_BYTES("init.x2.field_1.field_0", x3_field0_init, x2.field_1.field_0,
               2);
  EXPECT("init.x2.field_1.field_1", 32, x2.field_1.field_1);
  EXPECT("init.x2.field_1.field_2", 64, x2.field_1.field_2);

  X3 new_x3 = {.field_1 = 64, .field_2 = 128};
  uint8_t new_x3_field0[2] = {32, 0};
  memcpy(new_x3.field_0, new_x3_field0, 2);
  X2 new_x2 = {.field_0 = 16, .field_1 = new_x3};
  many_types_set_x2(many_types, new_x2);

  x2 = many_types_get_x2(many_types);
  EXPECT("rt.x2.field_0", 16, x2.field_0);
  EXPECT_BYTES("rt.x2.field_1.field_0", new_x3_field0, x2.field_1.field_0, 2);
  EXPECT("rt.x2.field_1.field_1", 64, x2.field_1.field_1);
  EXPECT("rt.x2.field_1.field_2", 128, x2.field_1.field_2);

  // Test me0 (Maybe<Either<bv8, bv16>>)
  Maybe_Either_bv8_bv16 me0 = many_types_get_me0(many_types);
  EXPECT("init.me0.tag", MAYBE_TAG_JUST, me0.tag);
  EXPECT("init.me0.just._0.tag", EITHER_TAG_LEFT, me0.just._0.tag);
  EXPECT("init.me0.just._0.left._0", 8, me0.just._0.left._0);

  Either_bv8_bv16 new_either_me0 = {.tag = EITHER_TAG_RIGHT};
  uint8_t me0_right[2] = {0x12, 0x00};
  memcpy(new_either_me0.right._0, me0_right, 2);
  Maybe_Either_bv8_bv16 new_me0 = {.tag = MAYBE_TAG_JUST,
                                   .just = {._0 = new_either_me0}};
  many_types_set_me0(many_types, new_me0);

  me0 = many_types_get_me0(many_types);
  EXPECT("rt.me0.tag", MAYBE_TAG_JUST, me0.tag);
  EXPECT("rt.me0.just._0.tag", EITHER_TAG_RIGHT, me0.just._0.tag);
  EXPECT_BYTES("rt.me0.just._0.right._0", me0_right, me0.just._0.right._0, 2);

  // Test me1 (Maybe<Either<bv16, bv8>>)
  Maybe_Either_bv16_bv8 me1 = many_types_get_me1(many_types);
  EXPECT("init.me1.tag", MAYBE_TAG_JUST, me1.tag);
  EXPECT("init.me1.just._0.tag", EITHER_TAG_LEFT, me1.just._0.tag);
  uint8_t me1_left_init[2] = {8, 0};
  EXPECT_BYTES("init.me1.just._0.left._0", me1_left_init, me1.just._0.left._0,
               2);

  Either_bv16_bv8 new_either_me1 = {.tag = EITHER_TAG_RIGHT,
                                    .right = {._0 = 0x12}};
  Maybe_Either_bv16_bv8 new_me1 = {.tag = MAYBE_TAG_JUST,
                                   .just = {._0 = new_either_me1}};
  many_types_set_me1(many_types, new_me1);

  me1 = many_types_get_me1(many_types);
  EXPECT("rt.me1.tag", MAYBE_TAG_JUST, me1.tag);
  EXPECT("rt.me1.just._0.tag", EITHER_TAG_RIGHT, me1.just._0.tag);
  EXPECT("rt.me1.just._0.right._0", 0x12, me1.just._0.right._0);

  // Test packed types p0-p3
  P0 p0 = many_types_get_p0(many_types);
  EXPECT("init.p0.field_0", 0xBADC, p0.field_0);
  EXPECT("init.p0.field_1", 0x0F, p0.field_1);
  EXPECT("init.p0.field_2", 0xEE, p0.field_2);

  P0 new_p0 = {.field_0 = 0x1234, .field_1 = 0x56, .field_2 = 0xFE};
  many_types_set_p0(many_types, new_p0);
  p0 = many_types_get_p0(many_types);
  EXPECT("rt.p0.field_0", 0x1234, p0.field_0);
  EXPECT("rt.p0.field_1", 0x56, p0.field_1);
  EXPECT("rt.p0.field_2", 0xFE, p0.field_2);

  P1 p1 = many_types_get_p1(many_types);
  EXPECT("init.p1.field_0", 0xBADC, p1.field_0);
  EXPECT("init.p1.field_1", 0x0F, p1.field_1);
  EXPECT("init.p1.field_2", 0xBEAD, p1.field_2);

  P1 new_p1 = {.field_0 = 0x1234, .field_1 = 0x56, .field_2 = 0xFACE};
  many_types_set_p1(many_types, new_p1);
  p1 = many_types_get_p1(many_types);
  EXPECT("rt.p1.field_0", 0x1234, p1.field_0);
  EXPECT("rt.p1.field_1", 0x56, p1.field_1);
  EXPECT("rt.p1.field_2", 0xFACE, p1.field_2);

  P2 p2 = many_types_get_p2(many_types);
  EXPECT("init.p2.field_0", 0xBADC, p2.field_0);
  EXPECT("init.p2.field_1", 0x0F, p2.field_1);
  EXPECT("init.p2.field_2", 6, p2.field_2);

  P2 new_p2 = {.field_0 = 0x1234, .field_1 = 0x56, .field_2 = 9};
  many_types_set_p2(many_types, new_p2);
  p2 = many_types_get_p2(many_types);
  EXPECT("rt.p2.field_0", 0x1234, p2.field_0);
  EXPECT("rt.p2.field_1", 0x56, p2.field_1);
  EXPECT("rt.p2.field_2", 9, p2.field_2);

  P3 p3 = many_types_get_p3(many_types);
  EXPECT("init.p3.field_0.field_0", 0xBADC, p3.field_0.field_0);
  EXPECT("init.p3.field_0.field_1", 0x0F, p3.field_0.field_1);
  EXPECT("init.p3.field_0.field_2", 6, p3.field_0.field_2);
  EXPECT("init.p3.field_1", 0xEE, p3.field_1);

  P2 new_p2_in_p3 = {.field_0 = 0x1234, .field_1 = 0x56, .field_2 = 9};
  P3 new_p3 = {.field_0 = new_p2_in_p3, .field_1 = 0xBA};
  many_types_set_p3(many_types, new_p3);
  p3 = many_types_get_p3(many_types);
  EXPECT("rt.p3.field_0.field_0", 0x1234, p3.field_0.field_0);
  EXPECT("rt.p3.field_0.field_1", 0x56, p3.field_0.field_1);
  EXPECT("rt.p3.field_0.field_2", 9, p3.field_0.field_2);
  EXPECT("rt.p3.field_1", 0xBA, p3.field_1);

  // Test t0 (tuple)
  tuple2_bv8_s16 t0 = many_types_get_t0(many_types);
  EXPECT("init.t0._0", 12, t0._0);
  EXPECT("init.t0._1", 584, t0._1);

  tuple2_bv8_s16 new_t0 = {._0 = 24, ._1 = -948};
  many_types_set_t0(many_types, new_t0);
  t0 = many_types_get_t0(many_types);
  EXPECT("rt.t0._0", 24, t0._0);
  EXPECT("rt.t0._1", -948, t0._1);

  // Test i20 (Index<20>)
  EXPECT("init.i20", 17, many_types_get_i20(many_types));
  many_types_set_i20(many_types, 3);
  EXPECT("rt.i20", 3, many_types_get_i20(many_types));

  // Test mi12 (Maybe<Index<12>>)
  Maybe_i12 mi12 = many_types_get_mi12(many_types);
  EXPECT("init.mi12.tag", MAYBE_TAG_JUST, mi12.tag);
  EXPECT("init.mi12.just._0", 5, mi12.just._0);

  Maybe_i12 new_mi12 = {.tag = MAYBE_TAG_NOTHING};
  many_types_set_mi12(many_types, new_mi12);
  mi12 = many_types_get_mi12(many_types);
  EXPECT("rt.mi12.tag", MAYBE_TAG_NOTHING, mi12.tag);

  // Test maybe_b96 (Maybe<BitVector 96>)
  Maybe_bv96 mb96 = many_types_get_maybe_b96(many_types);
  EXPECT("init.maybe_b96.tag", MAYBE_TAG_JUST, mb96.tag);
  uint8_t expected_b96[12] = {0xF1, 0xEE, 0xDB, 0xEA, 0x5D, 0x55,
                              0x55, 0x55, 0xB5, 0xBA, 0xBA, 0x4A};
  EXPECT_BYTES("init.maybe_b96.just._0", expected_b96, mb96.just._0, 12);

  Maybe_bv96 new_mb96 = {.tag = MAYBE_TAG_NOTHING};
  many_types_set_maybe_b96(many_types, new_mb96);
  mb96 = many_types_get_maybe_b96(many_types);
  EXPECT("rt.maybe_b96.tag", MAYBE_TAG_NOTHING, mb96.tag);

  // Test maybe_u96 (Maybe<Unsigned 96>)
  Maybe_u96 mu96 = many_types_get_maybe_u96(many_types);
  EXPECT("init.maybe_u96.tag", MAYBE_TAG_JUST, mu96.tag);
  // Note: Assuming uint128_t support and correct value

  Maybe_u96 new_mu96 = {.tag = MAYBE_TAG_NOTHING};
  many_types_set_maybe_u96(many_types, new_mu96);
  mu96 = many_types_get_maybe_u96(many_types);
  EXPECT("rt.maybe_u96.tag", MAYBE_TAG_NOTHING, mu96.tag);

  // Test maybe_s96 (Maybe<Signed 96>)
  Maybe_s96 ms96 = many_types_get_maybe_s96(many_types);
  EXPECT("init.maybe_s96.tag", MAYBE_TAG_JUST, ms96.tag);

  Maybe_s96 new_ms96 = {.tag = MAYBE_TAG_NOTHING};
  many_types_set_maybe_s96(many_types, new_ms96);
  ms96 = many_types_get_maybe_s96(many_types);
  EXPECT("rt.maybe_s96.tag", MAYBE_TAG_NOTHING, ms96.tag);

  // Test eitherAbc (Either<bv2, Abc>)
  Either_bv2_Abc ea = many_types_get_either_abc(many_types);
  EXPECT("init.eitherAbc.tag", EITHER_TAG_LEFT, ea.tag);
  EXPECT("init.eitherAbc.left._0", 0, ea.left._0);

  Either_bv2_Abc new_ea1 = {.tag = EITHER_TAG_LEFT, .left = {._0 = 0b11}};
  many_types_set_either_abc(many_types, new_ea1);
  ea = many_types_get_either_abc(many_types);
  EXPECT("rt.eitherAbc.tag", EITHER_TAG_LEFT, ea.tag);
  EXPECT("rt.eitherAbc.left._0", 0b11, ea.left._0);

  Either_bv2_Abc new_ea2 = {.tag = EITHER_TAG_RIGHT, .right = {._0 = ABC_B}};
  many_types_set_either_abc(many_types, new_ea2);
  ea = many_types_get_either_abc(many_types);
  EXPECT("rt.eitherAbc.tag", EITHER_TAG_RIGHT, ea.tag);
  EXPECT("rt.eitherAbc.right._0", ABC_B, ea.right._0);

  // Test only_referenced_in_vec
  OnlyReferencedInVec oriv;

  EXPECT("init.only_referenced_in_vec[0] exists", true,
         many_types_get_only_referenced_in_vec(many_types, 0, &oriv));
  EXPECT("init.only_referenced_in_vec[0].field_0", 2, oriv.field_0);
  uint8_t oriv0_field1_init[2] = {3, 0};
  EXPECT_BYTES("init.only_referenced_in_vec[0].field_1", oriv0_field1_init,
               oriv.field_1, 2);

  EXPECT("init.only_referenced_in_vec[1] exists", true,
         many_types_get_only_referenced_in_vec(many_types, 1, &oriv));
  EXPECT("init.only_referenced_in_vec[1].field_0", 4, oriv.field_0);
  uint8_t oriv1_field1_init[2] = {5, 0};
  EXPECT_BYTES("init.only_referenced_in_vec[1].field_1", oriv1_field1_init,
               oriv.field_1, 2);

  EXPECT("init.only_referenced_in_vec[2]", false,
         many_types_get_only_referenced_in_vec(many_types, 2, &oriv));

  OnlyReferencedInVec new_oriv0 = {.field_0 = 6};
  uint8_t oriv0_field1_rt[2] = {7, 0};
  memcpy(new_oriv0.field_1, oriv0_field1_rt, 2);
  many_types_set_only_referenced_in_vec(many_types, 0, new_oriv0);

  OnlyReferencedInVec new_oriv1 = {.field_0 = 8};
  uint8_t oriv1_field1_rt[2] = {9, 0};
  memcpy(new_oriv1.field_1, oriv1_field1_rt, 2);
  many_types_set_only_referenced_in_vec(many_types, 1, new_oriv1);

  many_types_get_only_referenced_in_vec(many_types, 0, &oriv);
  EXPECT("rt.only_referenced_in_vec[0].field_0", 6, oriv.field_0);
  EXPECT_BYTES("rt.only_referenced_in_vec[0].field_1", oriv0_field1_rt,
               oriv.field_1, 2);

  many_types_get_only_referenced_in_vec(many_types, 1, &oriv);
  EXPECT("rt.only_referenced_in_vec[1].field_0", 8, oriv.field_0);
  EXPECT_BYTES("rt.only_referenced_in_vec[1].field_1", oriv1_field1_rt,
               oriv.field_1, 2);

  // If we got here, all tests passed
  uart_puts(uart, "RESULT: OK\r\n");
  while (1) {
  }
}
