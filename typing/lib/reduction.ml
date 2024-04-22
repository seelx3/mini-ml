exception Nomilization_error

let rec reduce (e : Syntax.prog) : Syntax.prog option =
  match e with
  | Syntax.Add (n1, n2) -> (
    match (n1, n2) with
    | Syntax.Int n1, Syntax.Int n2 -> Some (Syntax.Int (n1 + n2))
    | _, _ -> (
      match reduce n1 with
      | Some n1' -> Some (Syntax.Add (n1', n2))
      | None -> (
        match reduce n2 with
        | Some n2' -> Some (Syntax.Add (n1, n2'))
        | None -> None)))
  | Syntax.Lt (n1, n2) -> (
    match (n1, n2) with
    | Syntax.Int n1, Syntax.Int n2 -> Some (Syntax.Bool (n1 < n2))
    | _, _ -> (
      match reduce n1 with
      | Some n1' -> Some (Syntax.Lt (n1', n2))
      | None -> (
        match reduce n2 with
        | Some n2' -> Some (Syntax.Lt (n1, n2'))
        | None -> None)))
  | Syntax.If (b, e1, e2) -> (
    match b with
    | Syntax.Bool b -> ( match b with true -> Some e1 | false -> Some e2)
    | _ -> (
      match reduce b with
      | Some b' -> Some (Syntax.If (b', e1, e2))
      | None -> None))
  | Syntax.Product (n1, n2) -> (
    match (n1, n2) with
    | _, _ -> (
      match reduce n1 with
      | Some n1' -> Some (Syntax.Product (n1', n2))
      | None -> (
        match reduce n2 with
        | Some n2' -> Some (Syntax.Product (n1, n2'))
        | None -> None)))
  | Syntax.Fst n -> (
    match n with
    | Syntax.Product (n1, _) -> Some n1
    | _ -> (
      match reduce n with Some n' -> Some (Syntax.Fst n') | None -> None))
  | Syntax.Snd n -> (
    match n with
    | Syntax.Product (_, n2) -> Some n2
    | _ -> (
      match reduce n with Some n' -> Some (Syntax.Snd n') | None -> None))
  | _ -> None

let normalize (e : Syntax.prog) : Syntax.prog =
  let rec normalize' (e : Syntax.prog) : Syntax.prog =
    match reduce e with Some e' -> normalize' e' | None -> e
  in
  normalize' e
