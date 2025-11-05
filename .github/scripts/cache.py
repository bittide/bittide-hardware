#!/usr/bin/env python3
"""
Strict caching script, that uploads to a hardcoded S3 server. Users should set
an environment variable 'S3_PASSWORD'.

Usage:
  cache push (cabal|dist-newstyle|cargo|build|clash|synth|build-post-synth) [--prefix=<prefix>] [--empty-pattern-ok] [--write-cache-found] [--overwrite-ok]
  cache pull (cabal|dist-newstyle|cargo|build|clash|synth|build-post-synth) [--prefix=<prefix>] [--missing-ok] [--overwrite-ok] [--write-cache-found]
  cache clean
  cache -h | --help
  cache --version

Options:
  -h --help            Show this screen.
  --version            Show version.
  --missing-ok         Do not throw an error if a cache is missing
  --overwrite-ok       Allow file overwrites on pull, or cache overwrites on push
  --write-cache-found  Writes '0' or '1' to 'cache_found', depending on whether a cache existed
  --empty-pattern-ok   Allow patterns to not match any files
  --prefix=<prefix>    Add prefix to the cache key
"""

# SPDX-FileCopyrightText: 2022 Google LLC
#
# SPDX-License-Identifier: Apache-2.0

import datetime
import dateutil.tz
import docopt
import functools
import glob
import hashlib
import itertools
import os
import random
import string
import subprocess
import sys

from typing import List, Iterable

PWD = os.getcwd()

GITHUB_ORG="bittide"
GITHUB_REPO="bittide-hardware"

CACHE_URL="http://hertme.local:9000"
CACHE_USER="root"
CACHE_BUCKET=f"github.org/{GITHUB_ORG}/{GITHUB_REPO}"

CLEAR_AFTER_DAYS=7
CLEAR_AFTER=f"{CLEAR_AFTER_DAYS}d00h00m00s"
TOUCH_AFTER=datetime.timedelta(days=1)

GLOBAL_CACHE_BUST = 12

CARGO_CACHE_BUST = 3
CARGO_KEY_PREFIX = f"cargo-g{GLOBAL_CACHE_BUST}-l{CARGO_CACHE_BUST}-"
CARGO_KEY_PATTERNS = ("**/Cargo.lock", "**/Cargo.toml", "**/rust-toolchain.toml")
CARGO_CACHE_INCLUDE_PATTERNS = (
    "~/.cargo",
    f"{PWD}/_build/cargo/",
)
CARGO_CACHE_EXCLUDE_PATTERNS = ()

CABAL_CACHE_BUST = 2
CABAL_KEY_PREFIX = f"cabal-g{GLOBAL_CACHE_BUST}-l{CABAL_CACHE_BUST}-"
CABAL_KEY_PATTERNS = ("**/cabal.project", "**/cabal.project.freeze")
CABAL_CACHE_INCLUDE_PATTERNS = (
    "~/.cabal-nix",
    f"{PWD}/dist-newstyle/src",
)
CABAL_CACHE_EXCLUDE_PATTERNS = ()

DIST_NEWSTYLE_CACHE_BUST = 1
DIST_NEWSTYLE_KEY_PREFIX = f"dist-newstyle-g{GLOBAL_CACHE_BUST}-l{DIST_NEWSTYLE_CACHE_BUST}-"
DIST_NEWSTYLE_KEY_PATTERNS = (*CABAL_KEY_PATTERNS, "**/*.cabal", "**/*.hs")
DIST_NEWSTYLE_CACHE_INCLUDE_PATTERNS = (
    f"{PWD}/dist-newstyle",
    f"{PWD}/_build/memory_maps",
    f"{PWD}/clash-vexriscv/clash-vexriscv/build_out_dir/",
)
DIST_NEWSTYLE_CACHE_EXCLUDE_PATTERNS = CABAL_CACHE_INCLUDE_PATTERNS

BUILD_CACHE_BUST = 2
BUILD_KEY_PREFIX = f"build-products-g{GLOBAL_CACHE_BUST}-l{BUILD_CACHE_BUST}-"
BUILD_CACHE_INCLUDE_PATTERNS = (
    f"{PWD}/_build/",
    f"{PWD}/clash-vexriscv/clash-vexriscv/build_out_dir/",
    f"{PWD}/dist-newstyle/",
    f"{PWD}/firmware-support/bittide-hal/src/shared/",
    f"{PWD}/firmware-support/bittide-hal/src/hals/",
)
BUILD_CACHE_EXCLUDE_PATTERNS = (
    f"{PWD}/firmware-support/bittide-hal/src/shared/mod.rs",
    f"{PWD}/dist-newstyle/src",
)

CLASH_CACHE_BUST = 0
CLASH_KEY_PREFIX = f"clash-g{GLOBAL_CACHE_BUST}-l{CLASH_CACHE_BUST}-"
CLASH_KEY_PATTERNS = CABAL_KEY_PATTERNS + (
    "**/*.cabal",
    "**/*.hs",
    "**/*.hs-boot",
    "**/*.csv",
)
CLASH_CACHE_INCLUDE_PATTERNS = (
    f"{PWD}/_build/clash",
)
CLASH_CACHE_EXCLUDE_PATTERNS = ()

SYNTH_CACHE_BUST = 2
SYNTH_KEY_PREFIX = f"synth-g{GLOBAL_CACHE_BUST}-l{SYNTH_CACHE_BUST}-"
SYNTH_KEY_PATTERNS = (
    f"**/bittide-instances/data/constraints/**/*.xdc",
    f"**/bittide-shake/**/*",
)
SYNTH_KEY_PATTERNS_UNTRACKED = (
    f"**/_build/clash/*/*.sdc",
    f"**/_build/clash/*/*.tcl",
    f"**/_build/clash/*/*.v",
)
SYNTH_CACHE_INCLUDE_PATTERNS = (
    f"{PWD}/_build/vivado",
)
SYNTH_CACHE_EXCLUDE_PATTERNS = ()

BUILD_POST_SYNTH_CACHE_BUST = 2
BUILD_POST_SYNTH_KEY_PREFIX = f"build-products-post-synth-g{GLOBAL_CACHE_BUST}-l{BUILD_POST_SYNTH_CACHE_BUST}-"
BUILD_POST_SYNTH_CACHE_INCLUDE_PATTERNS = (
    f"{PWD}/_build/clash",
    f"{PWD}/_build/vivado",
)
BUILD_POST_SYNTH_CACHE_EXCLUDE_PATTERNS = ()

def log(msg):
    """A poor man's logging function"""
    print(f"[{datetime.datetime.now().isoformat()}] {msg}", file=sys.stderr)

@functools.lru_cache()
def get_all_git_files():
    output = subprocess.check_output(["git", "ls-files"]).decode()
    paths = (line.strip() for line in output.split("\n") if line.strip())
    return frozenset(paths)

def sha256sum_file(path : str) -> str:
    hash = hashlib.sha256()
    with open(path, "rb") as fp:
        hash.update(fp.read())
    digest = hash.hexdigest()
    log(f"{digest} {path}")
    return digest

def sha256sum_files(file_paths : Iterable[str]) -> str:
    hash = hashlib.sha256()
    for path in sorted(set(file_paths)):
        hash.update(sha256sum_file(path).encode())
    return hash.hexdigest()


def get_git_files_from_patterns(patterns):
    filesytem_files = get_files_from_patterns(patterns)
    git_files = get_all_git_files()
    return (p for p in filesytem_files if p in git_files)

def get_files_from_patterns(patterns):
    return itertools.chain.from_iterable(
        glob.glob(pattern, recursive=True) for pattern in patterns
    )

def get_key_from_patterns(patterns):
    return sha256sum_files(get_git_files_from_patterns(patterns))


def get_cargo_key():
    return CARGO_KEY_PREFIX + get_key_from_patterns(CARGO_KEY_PATTERNS)


def get_cabal_key():
    return CABAL_KEY_PREFIX + get_key_from_patterns(CABAL_KEY_PATTERNS)


def get_dist_newstyle_key():
    return DIST_NEWSTYLE_KEY_PREFIX + get_key_from_patterns(DIST_NEWSTYLE_KEY_PATTERNS)


def get_build_key():
    return BUILD_KEY_PREFIX + os.environ["GITHUB_SHA"]

def get_clash_key():
    exclude = (
        "bittide-instances/src/Bittide/Instances/Hitl/Driver/",
        "bittide-instances/src/Bittide/Instances/Hitl/Utils/",
        "bittide-instances/src/Bittide/Instances/Hitl/Post/",
        "bittide-instances/bin",
        "gdb-hs/",
        "vivado-hs/",
    )
    files = get_git_files_from_patterns(CLASH_KEY_PATTERNS)
    files = (f for f in files if not f.startswith(exclude))
    return CLASH_KEY_PREFIX + sha256sum_files(files)

def get_synth_key():
    # Files in the _build directory are not tracked by git. Therefore we have
    # to get the list of files in separate steps.
    tracked_files = get_git_files_from_patterns(SYNTH_KEY_PATTERNS)
    untracked_files = get_files_from_patterns(SYNTH_KEY_PATTERNS_UNTRACKED)
    return SYNTH_KEY_PREFIX + sha256sum_files(itertools.chain(tracked_files, untracked_files))


def get_build_post_synth_key():
    return BUILD_POST_SYNTH_KEY_PREFIX + os.environ["GITHUB_SHA"]


def get_random_string():
    return ''.join(random.choice(string.ascii_letters) for i in range(10))

def parse_ls_line(line):
    """
    Parse a string like:

       2023-10-25 13:45:51 CEST 9.5GiB STANDARD foo.tar.zst-JZVnMBIapA

    into its date and filename component.
    """
    line = line.replace("[", "").replace("]", "").strip()
    line_split_no_padding = filter(None, line.split(" "))
    year_month_day, hour_minute_seconds, timezone_name, _size, _status, *filename = line_split_no_padding
    hour, minute, seconds = map(int, hour_minute_seconds.split(":"))
    year, month, day = map(int, year_month_day.split("-"))
    filename = " ".join(filename)
    timezone = dateutil.tz.gettz(timezone_name)
    timestamp = datetime.datetime(year, month, day, hour, minute, seconds, tzinfo=timezone)
    return timestamp, filename

class Mc:
    def __init__(self, key, *, prefix=None):
        self.key = key
        if prefix is not None:
            self.key = f"{prefix}-{self.key}"
        self.password = os.environ["S3_PASSWORD"]
        self._run(["mc", "alias", "set", "cache", CACHE_URL, CACHE_USER, self.password])
        self._run(["mc", "mb", "--ignore-existing", self._get_bucket()])

    def _get_pretty_cmd(self, cmd):
        return ' '.join(cmd).replace(self.password, "<masked-password>")

    def _pipe(self, cmd0, cmd1):
        """
        Run two commands, with the first command's stdout piped into the second's
        command stdin. Throws a `CalledProcessError` if either command exits with
        a non-zero exit code.

        Prints executed command to stderr (with masked password).
        """
        cmds = cmd0 + ["|"] + cmd1
        log(self._get_pretty_cmd(cmds))

        cmd0_ps = subprocess.Popen(cmd0, stdout=subprocess.PIPE)
        try:
            output = subprocess.check_call(cmd1, stdin=cmd0_ps.stdout)
        finally:
            retcode = cmd0_ps.wait()

        if retcode:
            raise subprocess.CalledProcessError(retcode, cmd0_ps.args)

        return output

    def _run(self, cmd : List[str]):
        """
        Run a command with `check_output`.

        Prints executed command to stderr (with masked password).
        """
        log(self._get_pretty_cmd(cmd))
        return subprocess.check_output(cmd)

    def _get_filename(self):
        return f"{self.key}.tar.zst"

    def _get_bucket(self):
        return f"cache/{CACHE_BUCKET}"

    def _get_path(self):
        return f"{self._get_bucket()}/{self._get_filename()}"

    def ls(self):
        """
        List tuples of (last_modified_timestamp, filename). Excludes '/' that is
        usually returned by S3.
        """
        lines = self._run(["mc", "ls", self._get_bucket()]).decode()
        for line in lines.split("\n"):
            if line.strip():
                timestamp, filename = parse_ls_line(line)
                if filename != "/":
                    yield (timestamp, filename)

    def clean(self):
        """Remove all files in our bucket older than `CLEAR_AFTER`"""
        self._run([
            "mc", "rm", "--recursive", "--force",
             f"--older-than={CLEAR_AFTER}",
             self._get_bucket()
            ]
        )

    def _get_from_patterns(self, name, patterns, empty_pattern_ok=False):
        """
        Get a list of paths from the filesystem matching the given patterns.
        For every pattern, if no paths are found, raise an error unless
        `empty_pattern_ok` is set.
        """
        for pattern in patterns:
            pattern = os.path.expanduser(pattern)
            found = glob.glob(pattern, recursive=True)
            if not found and not empty_pattern_ok:
                raise ValueError(f"{name.title()} pattern '{pattern}' did not match any files")
            yield from found

    def push(self, include_patterns, exclude_patterns, empty_pattern_ok=False, replace=False) -> bool:
        """
        Upload files in `patterns`. Will error if none of the patterns yielded
        any files. Will also error if _any_ pattern came up empty, unless
        `empty_pattern_ok` is set.

        Will not overwrite existing caches, unless `replace` is set. Returns
        'True' if the cache already existed or if a cache was written.
        """
        if not replace and self.stat() is not None:
            return False

        includes = list(self._get_from_patterns("include", include_patterns, empty_pattern_ok))
        if not includes:
            raise ValueError("No include patterns matched: unable to create cache")

        excludes = list(self._get_from_patterns("exclude", exclude_patterns, empty_pattern_ok))
        exclude_args = [f"--exclude={e}" for e in excludes]

        self._pipe(
            ["tar"] + exclude_args + ["--zstd", "-C", "/", "-cf", "-"] + sorted(includes),
            ["mc", "pipe", self._get_path()]
        )

        return True

    def touch(self, older_than=TOUCH_AFTER):
        """
        Renew a file's last modification date. Used to keep frequently used files
        around. Because this is a fairly expensive operation, we only perform it
        on files older than `TOUCH_AFTER` by default.
        """
        timestamp = self.stat()

        if timestamp is None:
            # File does not exist
            return

        if datetime.datetime.now().astimezone() - timestamp <= older_than:
            # File not old enough
            return

        filename = self._get_path()
        tmp_filename = f"{filename}-{get_random_string()}"

        self._run(["mc", "cp", filename, tmp_filename])
        self._run(["mc", "mv", tmp_filename, filename])

    def stat(self):
        """Return last modified stat or None if key does not exist"""
        lines = self._run(["mc", "ls", self._get_path()]).decode()
        filename = self._get_filename()
        for line in lines.split("\n"):
            if line.strip():
                listed_timestamp, listed_filename = parse_ls_line(line)
                if filename == listed_filename:
                    return listed_timestamp

    def pull(self, missing_ok=False, overwrite_ok=False) -> bool:
        """
        Retrieve and extract a file. Returns a bool indicating whether a pull
        failed. Note that the function will error if `missing_ok` is _not_ set
        and the cache does _not_ exist, instead of simply returning a boolean.
        """
        # Renew 'last modified', which we use as an indication which caches get
        # used frequently. Doesn't do anything if the file does not exist.
        self.touch()

        if missing_ok:
            if self.stat() is None:
                return False

        # Though it might seem that a file could get deleted in between the call
        # to `stat()` and the call to `_pipe()`, this shouldn't happen as we called
        # `touch()` at the very start of this function - caches only get cleaned
        # after `CLEAR_AFTER`.
        self._pipe(
            ["mc", "cat", self._get_path()],
            ["tar", "--zstd", "-C", "/", "-x"] + ([] if overwrite_ok else ["--keep-old-files"])
        )

        return True

def write_cache_result(result : bool):
    with open("cache_found", "w") as fp:
        fp.write("1" if result else "0")


def main(opts):

    if opts["cabal"]:
        key = get_cabal_key()
        include_patterns = CABAL_CACHE_INCLUDE_PATTERNS
        exclude_patterns = CABAL_CACHE_EXCLUDE_PATTERNS
    elif opts["dist-newstyle"]:
        key = get_dist_newstyle_key()
        include_patterns = DIST_NEWSTYLE_CACHE_INCLUDE_PATTERNS
        exclude_patterns = DIST_NEWSTYLE_CACHE_EXCLUDE_PATTERNS
    elif opts["cargo"]:
        key = get_cargo_key()
        include_patterns = CARGO_CACHE_INCLUDE_PATTERNS
        exclude_patterns = CARGO_CACHE_EXCLUDE_PATTERNS
    elif opts["build"]:
        key = get_build_key()
        include_patterns = BUILD_CACHE_INCLUDE_PATTERNS
        exclude_patterns = BUILD_CACHE_EXCLUDE_PATTERNS
    elif opts["clash"]:
        key = get_clash_key()
        include_patterns = CLASH_CACHE_INCLUDE_PATTERNS
        exclude_patterns = CLASH_CACHE_EXCLUDE_PATTERNS
    elif opts["synth"]:
        key = get_synth_key()
        include_patterns = SYNTH_CACHE_INCLUDE_PATTERNS
        exclude_patterns = SYNTH_CACHE_EXCLUDE_PATTERNS
    elif opts["build-post-synth"]:
        key = get_build_post_synth_key()
        include_patterns = BUILD_POST_SYNTH_CACHE_INCLUDE_PATTERNS
        exclude_patterns = BUILD_POST_SYNTH_CACHE_EXCLUDE_PATTERNS
    elif opts["clean"]:
        key = None
        include_patterns = ()
        exclude_patterns = ()
    else:
        raise ValueError("Unrecognized name")

    mc = Mc(key, prefix=opts["--prefix"])

    if opts["clean"]:
        mc.clean()

    elif opts["pull"]:
        cache_result = mc.pull(opts["--missing-ok"], opts["--overwrite-ok"])
        if not cache_result:
            print(f"Cache not found: {mc._get_filename()}")
        else:
            print(f"Cache found and extracted: {mc._get_filename()}")

        if opts["--write-cache-found"]:
            write_cache_result(cache_result)

    elif opts["push"]:
        cache_result = mc.push(
            include_patterns,
            exclude_patterns,
            opts["--empty-pattern-ok"],
            opts["--overwrite-ok"]
        )
        if not cache_result:
            print(f"Cache hit occured on the key {mc._get_filename()}, not saving cache")
        else:
            print(f"Saved cache to {mc._get_filename()}")

        if opts["--write-cache-found"]:
            write_cache_result(cache_result)
    else:
        raise ValueError(f"Unrecognized option in {opts}")


if __name__ == '__main__':
    opts = docopt.docopt(__doc__)
    main(opts)
