(* TODO: define a function `reduce` which performs exactly single step reduction *)

exception Nomilization_error

let rec normalize (e : Syntax.prog) : Syntax.prog =
  match e with
  | Syntax.Add (n1, n2) -> (
    match (normalize n1, normalize n2) with
    | Syntax.Int n1, Syntax.Int n2 -> Syntax.Int (n1 + n2)
    | _, _ -> raise Nomilization_error)
  | Syntax.Lt (n1, n2) -> (
    match (normalize n1, normalize n2) with
    | Syntax.Int n1, Syntax.Int n2 -> Syntax.Bool (n1 < n2)
    | _, _ -> raise Nomilization_error)
  | Syntax.If (b, e1, e2) -> (
    match b with
    | Syntax.Bool b -> (
      match b with true -> normalize e1 | false -> normalize e2)
    | _ -> normalize (Syntax.If (normalize b, normalize e1, normalize e2)))
  | _ -> e
