/*
SPDX-FileCopyrightText: 2025 Google LLC
SPDX-License-Identifier: CC0-1.0
*/

MEMORY
{
  IMEM : ORIGIN = 0x80000000, LENGTH = 64K
  DMEM : ORIGIN = 0xC0000000, LENGTH = 64K
}

REGION_ALIAS("REGION_TEXT", IMEM);
REGION_ALIAS("REGION_RODATA", DMEM);
REGION_ALIAS("REGION_DATA", DMEM);
REGION_ALIAS("REGION_BSS", DMEM);
REGION_ALIAS("REGION_HEAP", DMEM);
REGION_ALIAS("REGION_STACK", DMEM);
