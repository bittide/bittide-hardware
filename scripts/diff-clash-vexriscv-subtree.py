#!/usr/bin/env python3

# Show differences between committed subtree and current version.
#
# Usage:
#
#   ./diff-clash-vexriscv-subtree.py
#

# SPDX-FileCopyrightText: 2022-2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

import os
import re
import tempfile

from subprocess import check_output, check_call, run

HEX_RE = "[a-f0-9]+"
SUBTREE_RE = re.compile(f"Squashed 'clash-vexriscv/' changes from (?P<from>{HEX_RE})..(?P<to>{HEX_RE})")

CLASH_VEXRISCV_REPO = "https://github.com/clash-lang/clash-vexriscv.git"

def get_git_root():
    return check_output(["git", "rev-parse", "--show-toplevel"], text=True).strip()

def git_log_clash_vexriscv():
    return check_output(["git", "log", "--oneline", "clash-vexriscv"], text=True)

def get_latest_clash_vexriscv_commit():
    for line in git_log_clash_vexriscv().splitlines():
        match = SUBTREE_RE.search(line)
        if match:
            return match.group("to")

def diff(dir1, dir2):
    cmd = ["diff", "-ru", dir1, dir2]
    run(cmd, check=True)

def main():
    commit = get_latest_clash_vexriscv_commit()

    with tempfile.TemporaryDirectory() as tmpdir:
        check_call(["git", "clone", CLASH_VEXRISCV_REPO, tmpdir])
        check_call(["git", "checkout", commit], cwd=tmpdir)
        diff(tmpdir, "clash-vexriscv")

if __name__ == "__main__":
    os.chdir(get_git_root())
    main()
