# mini-ml

## About

- [WIP] ミニ関数型言語 with 型検査

## How to build

```
dune build
```

## How to run

```
chmod +x miniml
./miniml
```

- press Ctrl+D to exit.

## For developers
### How to debug

```
dune utop
```

- Check `typable` function like this:

```
utop # Miniml.Infer.infer (Miniml.Parse.parse "if (1 + 2) < 3 then 4 else 5");;
- : Miniml.Infer.typ = Miniml.Infer.TInt
```

- Check `normalize` function like this:

```
utop # Miniml.Reduction.normalize (Miniml.Parse.parse "if (1 + 2) < 3 then 4 else 5");;
- : Miniml.Syntax.prog = Miniml.Syntax.Int 5
```

### How to format

```
dune fmt
```

## References

- [Miniml a simple programming language - Samuel Mimram](https://www.lix.polytechnique.fr/Labo/Samuel.Mimram/teaching/INF551/TD/1.miniml.html)
- [関数プログラミング - 亀山幸義](http://logic.cs.tsukuba.ac.jp/jikken/index.html)
