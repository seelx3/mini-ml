# typing

## how to build

```
dune build
```

## how to run main.ml

```
dune exec typing
```

## how to use repl

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

## how to format

```
dune fmt
```
