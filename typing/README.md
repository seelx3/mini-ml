# typing

## how to build

```
dune build
```

## how to run

```
dune exec typing
```

## how to use repl

```
dune utop
```

- You can check `typable` expression like this:

```
utop # Typing.Infer.infer (Typing.Parse.parse "if (1 + 2) < 3 then 4 else 5");;
- : Typing.Infer.typ = Typing.Infer.TInt
```

## how to format
```
dune fmt
```