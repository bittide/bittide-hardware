<!--
SPDX-FileCopyrightText: 2025 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

# Haskell
In order to build a Haskell package, run:

```bash
cabal build -v0 ${package}
```

Refrain from using `tail` / `head` on the output of this command -- `-v0` should
suppress all extraneous output. Optionally, you can speed it up with:

```bash
cabal build -v0 ${package} --disable-tests
```

You can build everything with:

```bash
cabal build -v0 all
```

## Dependencies
You probably want to look around in `clash-prelude`, `clash-cores` and `clash-protocols`
some of the time. You can (currently) find these files in `dist-newstyle/src` after
building any package that depends on them or running `cabal update`.

## Development
Don't use `RecordWildCards` in new code. Use `NamedFieldPuns` instead. Or, if that's
too much, use `OverloadedRecordDot`. Both of these should be enabled by default. If
a new function has "too many" arguments or results, consider grouping them into
records.

# Rust
Before building a Rust crate, you **must** ensure that `bittide-instances` has
been built. You can do this by running:

```bash
cabal build -v0 bittide-instances
```

Then you can build the Rust crate using `cargo` as usual.
