#!/usr/bin/env python3
"""
Make sure all files have an up-to-date copyright notice.
"""

# SPDX-FileCopyrightText: 2024 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

import re
import sys
import git

from typing import Set, Tuple

SPDX_MAGIC = "SPDX-FileCopyrightText:"

YEAR_RE = re.compile(r"\d{4}")

IGNORE_STARTSWITH = (
    "clash-vexriscv",
)

def iter_change_types(diff_index, change_types):
    for change_type in change_types:
        yield from diff_index.iter_change_type(change_type)

def get_pr_commits(repo, common_ancestor):
    """
    Gets all commits from the current one to (but not including) a common ancestor
    given as an argument.
    """
    for commit in repo.iter_commits():
        if commit.binsha == common_ancestor.binsha:
            break
        yield commit

def get_relevant_files_from_diff_index(diff_index):
    """
    Get all filepaths that are added, modified, or renamed in a DiffIndex.
    """
    for diff in iter_change_types(diff_index, ("A", "M", "R")):
        yield diff.b_path

def get_copyright_years(repo, commit, path):
    content = repo.git.show(f"{commit.hexsha}:{path}")
    lines = content.split("\n")

    for lineno, line in enumerate(lines, start=1):
        if SPDX_MAGIC in line:
            matches = YEAR_RE.findall(line)
            if not matches:
                # XXX: We don't error on "poorly formatted" SPDX headers, as our
                #      current check also triggers on things like our SPDX_MAGIC
                #      definition.
                continue
            yield int(matches[-1]), lineno

def is_valid_commit_file(repo, commit, path) -> bool:
    """
    Checks whether a specific file in a specific commit has up-to-date copyright
    notices.
    """
    commit_year = commit.authored_datetime.year
    years_and_line_nos = list(get_copyright_years(repo, commit, path))

    if not years_and_line_nos:
        return True

    max_year, line_no = max(years_and_line_nos)

    if max_year != commit_year:
        print(f"{commit.hexsha} {path}:{line_no} Unexpected copyright year. Expected {commit_year}, got {max_year}.", file=sys.stderr)
        return False

    return True


def is_valid_commit(repo, commit, seen_files) -> Tuple[bool, Set[str]]:
    """
    Check whether all files touched in given commit have up-to-date copyright
    notices. Ignores `seen_files` to make sure only the oldest commits introducing
    an issue get listed.

    Returns a tuple of (everything_ok, checked_files)
    """
    if not commit.parents:
        print("Ignoring root commit..", file=sys.stderr)
        return True, set()

    parent = commit.parents[0]
    files = set(get_relevant_files_from_diff_index(parent.diff(commit)))
    files -= seen_files
    files = {f for f in files if not f.startswith(IGNORE_STARTSWITH)}

    all_valid = True
    for file in files:
        try:
            all_valid &= is_valid_commit_file(repo, commit, file)
        except:
            raise ValueError(f"Unexpected exception when validating {commit.hexsha}:{file}")

    return all_valid, files


def main(merge_base_branch):
    repo = git.Repo(".")

    # Get the last common ancestor of the current branch and the merge base
    current_branch = repo.active_branch
    common_ancestor_hexdigest = repo.git.merge_base(merge_base_branch, current_branch.name)
    common_ancestor = repo.commit(common_ancestor_hexdigest)

    if merge_base_branch == "origin/staging" and current_branch == "staging":
        return

    if current_branch.commit.binsha == common_ancestor.binsha:
        exit(f"No commits on top of base branch '{merge_base_branch}'. Empty PR?")

    # Get the commits that are in the current branch but not in the merge target
    pr_commits = get_pr_commits(repo, common_ancestor)

    # Check if the year in the license header is correct for each file in each commit
    all_valid = True
    seen = set()
    for commit in pr_commits:
        commit_valid, files_in_commit = is_valid_commit(repo, commit, seen)
        all_valid &= commit_valid
        seen |= files_in_commit

    # Exit with error if not all copyright notices were valid
    if not all_valid:
        exit("Incorrect year found in some files. Please update the year in the license header.")

if __name__ == "__main__":
    try:
        base_branch = sys.argv[1]
    except IndexError:
        base_branch = "origin/staging"

    main(base_branch)
