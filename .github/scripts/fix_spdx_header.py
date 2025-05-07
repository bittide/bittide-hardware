#!/usr/bin/env python3
"""
Make sure the SPDX header is at the top of a given file. This is currently
hardcoded to look for the SPDX header formatted in a Haskell comment. This works
around an issue where Fourmolu thinks an SPDX header is part of a comment on a
language pragma, and wrongly produces formatting issues.
"""

# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

import os
import sys

SPDX_START = "-- SPDX-FileCopyrightText"
SPDX_END = "-- SPDX-License-Identifier"

def format(fp_src, fp_dst):
    source_lines = []
    spdx_lines = []

    for line in fp_src:
        if line.startswith(SPDX_START):
            # Collect SPDX header
            spdx_lines.append(line)
            for line in fp_src:
                spdx_lines.append(line)
                if line.startswith(SPDX_END):
                    break
            break
        else:
            # Collect non-SPDX header lines
            source_lines.append(line)

    # Write out SPDX header
    for spdx_line in spdx_lines:
        fp_dst.write(spdx_line)

    # Write out all non-SPDX header lines
    for source_line in source_lines:
        fp_dst.write(source_line)

    # Write out rest of the file
    for line in fp_src:
        fp_dst.write(line)

def main(paths):
    for path in paths:
        with open(path) as fp_src:
            with open(path + ".fix-spdx-header", "w") as fp_dst:
                format(fp_src, fp_dst)
        os.rename(path + ".fix-spdx-header", path)

if __name__ == '__main__':
    main(sys.argv[1:])

