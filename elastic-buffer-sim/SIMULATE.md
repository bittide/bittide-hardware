Simulation is intensive, so you likely want to run

```
cabal run exe:sim -w ghc-9.0.2 -- 2000000
```

rather than using the REPL.

Then:

```
python3 script.py
```

This will generate `clocks.png` and `elasticbuffers.png`.
