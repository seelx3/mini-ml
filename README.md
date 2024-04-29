# simple-prog-lang

## About

- [WIP] ミニ関数型言語 with 型検査

## How to build

```
dune build
```

## How to run main.ml

```
dune exec typing
```

## How to use repl

```
dune utop
```

- Check `typable` function like this:

```
utop # Typing.Infer.infer (Typing.Parse.parse "if (1 + 2) < 3 then 4 else 5");;
- : Typing.Infer.typ = Typing.Infer.TInt
```

- Check `normalize` function like this:

```
utop # Typing.Reduction.normalize (Typing.Parse.parse "if (1 + 2) < 3 then 4 else 5");;
- : Typing.Syntax.prog = Typing.Syntax.Int 5
```

## How to format

```
dune fmt
```

## References

- [Typing a simple programming language](https://www.lix.polytechnique.fr/Labo/Samuel.Mimram/teaching/INF551/TD/1.typing.html)
