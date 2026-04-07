#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

"""Sweep gain values in ClockControl/Config.hs, committing and pushing each."""

import re
import subprocess
import sys

CONFIG_PATH = "bittide/src/Bittide/ClockControl/Config.hs"
GAIN_PATTERN = re.compile(r"(gain\s*=\s*)[\d.]+e-?\d+")


def gain_values():
    """Generate gain values from 1e-9 to 9e-7."""
    for exp in range(9, 7 - 1, -1):  # 9, 8, 7
        for mantissa in range(1, 10):
            yield mantissa, exp


def format_gain(mantissa, exp):
    return f"{mantissa}e-{exp}"


def run(cmd, **kwargs):
    print(f"$ {' '.join(cmd)}")
    subprocess.run(cmd, check=True, **kwargs)


def main():
    for mantissa, exp in gain_values():
        gain_str = format_gain(mantissa, exp)
        print(f"\n=== Setting gain to {gain_str} ===")

        # Read and update the file
        with open(CONFIG_PATH, "r") as f:
            content = f.read()

        new_content, n = GAIN_PATTERN.subn(rf"\g<1>{gain_str}", content)
        if n == 0:
            print(f"ERROR: Could not find gain pattern in {CONFIG_PATH}", file=sys.stderr)
            sys.exit(1)

        with open(CONFIG_PATH, "w") as f:
            f.write(new_content)

        # Commit and push
        run(["git", "add", CONFIG_PATH])
        run(["git", "commit", "-m", f"gain: {gain_str}"])
        run(["git", "push"])


if __name__ == "__main__":
    main()
