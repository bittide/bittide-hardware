<!--
SPDX-FileCopyrightText: 2022 Google LLC

SPDX-License-Identifier: Apache-2.0
-->

`Bittide.Topology` makes extensive use of Template Haskell. To get a handle on
what is actually generated, you can dump splices in the REPL, viz.

```haskell
putStrLn $(stringE . pprint =<< plotDats (complete 3))
```
