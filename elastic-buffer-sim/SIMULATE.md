<!--
SPDX-FileCopyrightText: 2022 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

Simulation is intensive, so you likely want to run

```
cabal run sim -- csv 2000000
```

rather than using the REPL.

Then, run

```
poetry run python genplots.py
```

to generate `clocks.pdf` and `elasticbuffers.pdf`.
