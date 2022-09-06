`Bittide.Topology` makes extensive use of Template Haskell. To get a handle on
what is actually generated, you can dump splices in the REPL, viz.

```haskell
putStrLn $(stringE . pprint =<< plotDats (complete 3))
```
