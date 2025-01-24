#!/usr/bin/env python3
"""
Usage:

    python3 update-vexriscv.py <commit_hash>
"""
# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

import glob
import os
import shutil
import subprocess
import sys
import tempfile

LICENSE = """
SPDX-FileCopyrightText: 2016 Spinal HDL contributors

SPDX-License-Identifier: MIT
""".lstrip()

VEXRISCV_REPO = "https://github.com/SpinalHDL/VexRiscv.git"

HERE = os.path.dirname(os.path.abspath(__file__))

def main(commit_hash):
    # Remove old JAR in current directory
    for file in os.listdir(HERE):
        if file.endswith((".jar", ".jar.license")):
            os.remove(os.path.join(HERE, file))

    with tempfile.TemporaryDirectory() as temp_dir:
        # Build new JAR
        subprocess.check_call(["git", "clone", VEXRISCV_REPO, temp_dir])
        subprocess.check_call(["git", "checkout", commit_hash], cwd=temp_dir)
        subprocess.check_call(["sbt", "package"], cwd=temp_dir)

        # Find and copy new JAR
        jar_path = glob.glob(os.path.join(temp_dir, "target", "scala-*", "vexriscv_*.jar"))[0]
        jar_filename = os.path.basename(jar_path)
        jar_base, jar_ext = os.path.splitext(jar_filename)
        new_jar_filename = f"{jar_base}-{commit_hash}{jar_ext}"
        shutil.copyfile(jar_path, os.path.join(HERE, new_jar_filename))

        # Write license file
        with open(os.path.join(HERE, f"{new_jar_filename}.license"), "w") as f:
            f.write(LICENSE)


if __name__ == '__main__':
    main(sys.argv[1])
