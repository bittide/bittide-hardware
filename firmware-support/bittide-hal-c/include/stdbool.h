/* SPDX-FileCopyrightText: 2025 Google LLC
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#ifndef STDBOOL_H
#define STDBOOL_H

/* Minimal stdbool.h for freestanding RISC-V environment */

#include "stdint.h"

/* Boolean */
typedef uint8_t bool;
#define true 1
#define false 0

#endif /* STDBOOL_H */
