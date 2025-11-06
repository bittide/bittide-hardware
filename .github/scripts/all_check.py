#!/usr/bin/env python3
"""
Makes sure:

 * All jobs are listed in the 'all' job
 * Only existing tests are listed

"""

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

import sys
import yaml

from typing import Dict, List, Set

CI_PATH = ".github/workflows/ci.yml"
ALL_TEST = "all"


def reachable(graph: Dict[str, List[str]], start: str) -> Set[str]:
    """Return all nodes reachable from start in the directed graph."""
    visited = set()
    stack = [start]

    while stack:
        node = stack.pop()
        if node not in visited:
            visited.add(node)
            stack.extend(graph.get(node, []))

    return visited - {start}


def main():
    ci_yml_fp = open(CI_PATH, "r")
    ci_yml_parsed = yaml.load(ci_yml_fp, Loader=yaml.FullLoader)

    job_graph = {
        job: ci_yml_parsed["jobs"][job].get("needs", [])
        for job in ci_yml_parsed["jobs"]
    }

    all_jobs = set(ci_yml_parsed["jobs"].keys()) - {ALL_TEST}
    all_direct_needs = set(ci_yml_parsed["jobs"][ALL_TEST].get("needs", []))
    all_needs = reachable(job_graph, ALL_TEST)
    all_indirect_needs = set.union(*(
        reachable(job_graph, job) for job in all_needs
    ))

    if all_jobs - all_needs:
        sys.exit(f"Not all jobs mentioned in {ALL_TEST}.needs: {all_jobs - all_needs}")

    if all_needs - all_jobs:
        sys.exit(f"Non-existing jobs found in {ALL_TEST}.needs: {all_needs - all_jobs}")

    if all_direct_needs & all_indirect_needs:
        sys.exit(f"Superfluous jobs found in {ALL_TEST}.needs: {all_direct_needs & all_indirect_needs}")


if __name__ == "__main__":
    main()
