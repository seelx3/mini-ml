type env = (string * Syntax.prog) list

let rec reduce (e : Syntax.prog) (env : env) : (Syntax.prog * env) option =
  match (e, env) with
  | Syntax.Var x, _ -> (
    match List.assoc_opt x env with Some e' -> Some (e', env) | None -> None)
  | Syntax.Add (n1, n2), _ -> (
    match (n1, n2) with
    | Syntax.Int n1, Syntax.Int n2 -> Some (Syntax.Int (n1 + n2), env)
    | _, _ -> (
      match reduce n1 env with
      | Some (n1', _) -> Some (Syntax.Add (n1', n2), env)
      | None -> (
        match reduce n2 env with
        | Some (n2', _) -> Some (Syntax.Add (n1, n2'), env)
        | None -> None)))
  | Syntax.Lt (n1, n2), env -> (
    match (n1, n2) with
    | Syntax.Int n1, Syntax.Int n2 -> Some (Syntax.Bool (n1 < n2), env)
    | _, _ -> (
      match reduce n1 env with
      | Some (n1', _) -> Some (Syntax.Lt (n1', n2), env)
      | None -> (
        match reduce n2 env with
        | Some (n2', _) -> Some (Syntax.Lt (n1, n2'), env)
        | None -> None)))
  | Syntax.If (b, e1, e2), env -> (
    match b with
    | Syntax.Bool b -> (
      match b with true -> Some (e1, env) | false -> Some (e2, env))
    | _ -> (
      match reduce b env with
      | Some (b', _) -> Some (Syntax.If (b', e1, e2), env)
      | None -> None))
  | Syntax.Product (n1, n2), env -> (
    match (n1, n2) with
    | _, _ -> (
      match reduce n1 env with
      | Some (n1', _) -> Some (Syntax.Product (n1', n2), env)
      | None -> (
        match reduce n2 env with
        | Some (n2', _) -> Some (Syntax.Product (n1, n2'), env)
        | None -> None)))
  | Syntax.Fst n, env -> (
    match n with
    | Syntax.Product (n1, _) -> Some (n1, env)
    | _ -> (
      match reduce n env with
      | Some (n', _) -> Some (Syntax.Fst n', env)
      | None -> None))
  | Syntax.Snd n, env -> (
    match n with
    | Syntax.Product (_, n2) -> Some (n2, env)
    | _ -> (
      match reduce n env with
      | Some (n', _) -> Some (Syntax.Snd n', env)
      | None -> None))
  | Syntax.Let (x, e1, e2), env -> 
    Some (e2, (x, e1) :: env)
  | _ -> None

let normalize (e : Syntax.prog) : Syntax.prog =
  let rec normalize' (e : Syntax.prog) (env : env) : Syntax.prog =
    match reduce e env with Some (e', env') -> normalize' e' env' | None -> e
  in
  normalize' e []
