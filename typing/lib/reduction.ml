(* TODO: define a function `reduce` which performs exactly single step reduction *)

let rec normalize (e : Syntax.prog) : Syntax.prog =
  match e with
  | Syntax.Add (n1, n2) -> (
    match (normalize n1, normalize n2) with
    | Syntax.Int n1, Syntax.Int n2 -> Syntax.Int (n1 + n2)
    | Syntax.Int n1, _ -> Syntax.Add (Syntax.Int n1, normalize n2)
    | _, Syntax.Int n2 -> Syntax.Add (normalize n1, Syntax.Int n2)
    | _, _ -> Syntax.Add (normalize n1, normalize n2))
  | Syntax.Lt (n1, n2) -> (
    match (normalize n1, normalize n2) with
    | Syntax.Int n1, Syntax.Int n2 -> Syntax.Bool (n1 < n2)
    | Syntax.Int n1, _ -> Syntax.Lt (Syntax.Int n1, normalize n2)
    | _, Syntax.Int n2 -> Syntax.Lt (normalize n1, Syntax.Int n2)
    | _, _ -> Syntax.Lt (normalize n1, normalize n2))
  | Syntax.If (b, e1, e2) -> (
    match b with
    | Syntax.Bool b -> (
      match b with true -> normalize e1 | false -> normalize e2)
    | _ -> normalize (Syntax.If (normalize b, normalize e1, normalize e2)))
  | _ -> e
