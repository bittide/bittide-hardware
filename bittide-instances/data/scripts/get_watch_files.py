#!/usr/bin/env python3
"""
Print all files tracked and untracked, but not ignored by git. Arguments passed
to this script are interpreted as git-style ignore patterns. E.g.:


    get_watch_files.py '*.md'

would return all tracked, untracked, and not ignored files minus all Markdown
files. Be careful when using this from the command line: your shell might do
globbing for you. Examples of supported patterns:

    README.md    (matches any README.md file)
    *.md         (matches all files ending in '.md')
    /.github     (matches .github folder in root of repo)

This is implemented in Python, because Haskell doesn't provide a satisfactory
implementation of Python's 'fnmatch'.
"""

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

import os
import sys
import subprocess

from fnmatch import fnmatch

def check_output_lines(args):
    """
    Like 'subprocess.check_output', but assumes command output is UTF-8 and
    returns whitespace cleaned, non-empty lines.
    """
    lines = subprocess.check_output(args, encoding="utf-8").split("\n")
    lines = [l.strip() for l in lines]
    return [l for l in lines if l]

def get_git_root():
    """Get root of current git repository"""
    return check_output_lines(["git", "rev-parse", "--show-toplevel"])[0]

def get_tracked_files():
    """Return files tracked by git"""
    return check_output_lines(["git", "ls-files"])

def get_untracked_files():
    """Return files untracked by git, but also not ignored"""
    lines = check_output_lines(["git", "status", "--short", "--untracked-files=all"])
    for line in lines:
        if line.startswith("?"):
            yield line.split(" ", 1)[1]

def get_path_parts(path):
    """Yield all parts that make up a path, in reverse order"""
    root, tail = os.path.split(path)
    if tail:
        yield tail
        yield from get_path_parts(root)
    elif root:
        yield root

def match(path, patterns):
    """Match an fnmatch pattern to a path, or any of its parts."""
    def go(pattern):
        if pattern.startswith("/"):
            yield path.startswith(pattern[1:])
            yield fnmatch(path, pattern[1:])
        else:
            yield fnmatch(path, pattern)
            yield any(fnmatch(part, pattern) for part in get_path_parts(path))

    for pattern in patterns:
        if any(go(pattern)):
            return True

if __name__ == '__main__':
    os.chdir(get_git_root())

    ignores = sys.argv[1:]
    tracked_files = list(get_tracked_files())
    untracked_files = list(get_untracked_files())
    all_files = sorted(set(tracked_files + untracked_files))
    for file in all_files:
        if not match(file, ignores):
            print(file)
