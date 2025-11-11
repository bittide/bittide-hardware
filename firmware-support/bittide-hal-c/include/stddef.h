/* SPDX-FileCopyrightText: 2025 Google LLC
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#ifndef STDDEF_H
#define STDDEF_H

/* Minimal stddef.h for freestanding RISC-V environment */

#include "stdint.h"

typedef uintptr_t size_t;
typedef intptr_t ptrdiff_t;

#define PTRDIFF_MIN INT32_MIN
#define PTRDIFF_MAX INT32_MAX
#define SIZE_WIDTH (sizeof(size_t))

#endif /* STDDEF_H */
