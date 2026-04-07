#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2025 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

"""Download CI 'Upload report' artifacts for each gain sweep commit."""

import json
import os
import subprocess
import sys


def gain_values():
    """Generate gain values from 1e-9 to 9e-7."""
    for exp in range(9, 7 - 1, -1):  # 9, 8, 7
        for mantissa in range(1, 10):
            yield mantissa, exp


def format_gain(mantissa, exp):
    return f"{mantissa}e-{exp}"


def run(cmd, capture=False):
    print(f"$ {' '.join(cmd)}")
    if capture:
        result = subprocess.run(cmd, check=True, capture_output=True, text=True)
        return result.stdout.strip()
    subprocess.run(cmd, check=True)


def main():
    out_dir = "reports"
    os.makedirs(out_dir, exist_ok=True)

    for mantissa, exp in gain_values():
        gain_str = format_gain(mantissa, exp)
        commit_msg = f"gain: {gain_str}"
        print(f"\n=== Downloading report for {gain_str} ===")

        # Find the commit SHA for this gain value
        sha = run(
            ["git", "log", "--all", "--format=%H", "--grep", commit_msg, "-1"],
            capture=True,
        )
        if not sha:
            print(f"WARNING: No commit found for '{commit_msg}', skipping")
            continue

        # Find the CI run for this commit
        runs_json = run(
            ["gh", "run", "list", "--commit", sha, "--json", "databaseId,status"],
            capture=True,
        )
        runs = json.loads(runs_json)
        if not runs:
            print(f"WARNING: No CI run found for commit {sha[:8]}, skipping")
            continue

        run_id = str(runs[0]["databaseId"])
        status = runs[0]["status"]
        print(f"  Run {run_id} (status: {status})")

        # Download all *-clock-report artifacts for this run
        dest = os.path.join(out_dir, gain_str)
        os.makedirs(dest, exist_ok=True)
        try:
            run(["gh", "run", "download", run_id, "-p", "*-clock-report", "-D", dest])
        except subprocess.CalledProcessError:
            print(f"WARNING: Failed to download artifacts for run {run_id}, skipping")
            continue

        print(f"  Saved to {dest}")


if __name__ == "__main__":
    main()
