#!/usr/bin/env python3
"""
Vivado produces ILA dumps as VCDs and CSVs with incredibly long probe names,
depending on the hardware design. This script shortens the names in the data
dumps to just their probe names, without any prefixes or bus suffixes.

This is written in Python because its currently not feasible to transfer Haskell
build products (executables) to our hardware-in-the-loop machine.
"""

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

import subprocess
import shutil
import glob
import os
import csv
import re

BUS_RE = re.compile(r'( *\[[0-9]+:[0-9]+\] *)*$')
VIVADO_DIR = "_build/vivado"

def check_output_lines(args):
    """
    Like 'subprocess.check_output', but assumes command output is UTF-8 and
    returns right-whitespace cleaned, non-empty lines.
    """
    lines = subprocess.check_output(args, encoding="utf-8").split("\n")
    lines = [l.rstrip() for l in lines]
    return [l for l in lines if l]

def get_git_root():
    """Get root of current git repository"""
    return check_output_lines(["git", "rev-parse", "--show-toplevel"])[0]

def remove_bus_suffix(s):
    """See `run_remove_bus_suffix_tests`"""
    return re.sub(BUS_RE, '', s)

def remove_prefix(s):
    """See `run_remove_prefix_tests`"""
    return s.rsplit("/", 1)[-1]

def get_ila_dumps(extension):
    """Get ILA data dumps with given extension"""
    return glob.glob(f"{VIVADO_DIR}/*/ila-data/*/*/*.{extension}")

def shorten_csv_names(path):
    """
    Remove probe prefixes and bus suffixes from probe names in a CSV file
    """
    with open(path, "r") as fp_src:
        with open(f"{path}.tmp", "w") as fp_dst:
            reader = csv.reader(fp_src, delimiter=',', quotechar='"')
            writer = csv.writer(fp_dst, delimiter=',', quotechar='"')
            header = [remove_prefix(remove_bus_suffix(c)) for c in next(reader)]
            writer.writerow(header)
            writer.writerows(reader)
    shutil.move(f"{path}.tmp", path)

def shorten_vcd_line(line):
    """
    If the given line is a VCD line introducing a variable name, remove probe
    prefixes and bus suffixes. If it is not, return line unchanged.
    """
    if not line.startswith("$var"):
        return line

    var, typ, width, id_, *name, end = line.split(" ")
    name = remove_prefix(remove_bus_suffix(" ".join(name)))
    assert end == "$end"
    assert var == "$var"
    assert width.isnumeric()

    return " ".join([var, typ, width, id_, name, end])

def shorten_vcd_names(path):
    """
    Remove probe prefixes and bus suffixes from probe names in a VCD file
    """
    with open(path, "r") as fp_src:
        with open(f"{path}.tmp", "w") as fp_dst:
            for line in fp_src:
                fp_dst.write(shorten_vcd_line(line.rstrip()) + "\n")
    shutil.move(f"{path}.tmp", path)

def main():
    os.chdir(get_git_root())

    for path in get_ila_dumps("csv"):
        shorten_csv_names(path)

    for path in get_ila_dumps("vcd"):
        shorten_vcd_names(path)

def run_remove_bus_suffix_tests():
    assert remove_bus_suffix("abc [3:0]") == "abc"
    assert remove_bus_suffix("abc [3:0] [4:0]") == "abc"
    assert remove_bus_suffix("abc [3:0][4:0]") == "abc"
    assert remove_bus_suffix("abc") == "abc"
    assert remove_bus_suffix("abc0") == "abc0"
    assert remove_bus_suffix("abc[0]") == "abc[0]"
    assert remove_bus_suffix("abc[a:0]") == "abc[a:0]"

def run_remove_prefix_tests():
    assert remove_prefix("abc") == "abc"
    assert remove_prefix("def/abc") == "abc"
    assert remove_prefix("xxx/def/abc") == "abc"
    assert remove_prefix("") == ""
    assert remove_prefix("/abc") == "abc"
    assert remove_prefix("//") == ""

if __name__ == '__main__':
    run_remove_bus_suffix_tests()
    run_remove_prefix_tests()
    main()
