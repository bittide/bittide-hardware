<!--
SPDX-FileCopyrightText: 2022 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

Simulation is computation intensive, so you likely want to run

```
cabal run sim -- plot 2000 1000
```

rather than using the REPL. This will generate `_build/clocks.pdf` and
`_build/elasticbuffers.pdf`

One can also dump to csv, viz.

```
cabal run sim -- csv 2000 1000
```

The python script is managed with [Poetry](https://python-poetry.org/).
First, [install Poetry](https://python-poetry.org/docs/#installation).

Set up the project with:

```
poetry install
```

Then, run

```
poetry run python genplots.py
```

to generate `_build/clocks.pdf` and `_build/elasticbuffers.pdf`.
