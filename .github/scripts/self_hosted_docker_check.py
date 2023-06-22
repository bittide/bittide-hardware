#!/usr/bin/env python3
"""
Make sure all self-hosted jobs run in a docker container
"""

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

import sys
import yaml

CI_PATH = ".github/workflows/ci.yml"
SELF_HOSTED_LABEL = "self-hosted"


def main():
    ci_yml_fp = open(CI_PATH, "r")
    ci_yml_parsed = yaml.safe_load(ci_yml_fp)

    jobs = ci_yml_parsed["jobs"]
    for job_name, job in jobs.items():
        runs_on = job["runs-on"]
        if SELF_HOSTED_LABEL in runs_on:
            try:
                job["container"]["image"]
            except KeyError:
                raise ValueError(f"self-hosted jobs should run in docker containers, '{job_name}' does not")


if __name__ == "__main__":
    main()
