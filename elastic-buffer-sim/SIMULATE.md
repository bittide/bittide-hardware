Simulation is intensive, so you likely want to run

```
cabal run sim -w ghc-9.0.2 -- csv 2000000
```

rather than using the REPL.

Then, run

```
python3 script.py
```

to generate `clocks.pdf` and `elasticbuffers.pdf`.
