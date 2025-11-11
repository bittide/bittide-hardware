/* SPDX-FileCopyrightText: 2025 Google LLC
 *
 * SPDX-License-Identifier: Apache-2.0
 */

#ifndef STDBOOL_H
#define STDBOOL_H

/* Minimal stdbool.h for freestanding RISC-V environment */

#define bool _Bool
#define true 1
#define false 0
#define __bool_true_false_are_defined 1

#endif /* STDBOOL_H */
