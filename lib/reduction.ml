open Syntax

type env = (string * prog) list

let rec reduce (e : prog) (env : env) : (prog * env) option =
  match (e, env) with
  | Var x, _ -> (
    match List.assoc_opt x env with Some e' -> Some (e', env) | None -> None)
  | Add (n1, n2), _ -> (
    match (n1, n2) with
    | Int n1, Int n2 -> Some (Int (n1 + n2), env)
    | _, _ -> (
      match reduce n1 env with
      | Some (n1', _) -> Some (Add (n1', n2), env)
      | None -> (
        match reduce n2 env with
        | Some (n2', _) -> Some (Add (n1, n2'), env)
        | None -> None)))
  | Lt (n1, n2), env -> (
    match (n1, n2) with
    | Int n1, Int n2 -> Some (Bool (n1 < n2), env)
    | _, _ -> (
      match reduce n1 env with
      | Some (n1', _) -> Some (Lt (n1', n2), env)
      | None -> (
        match reduce n2 env with
        | Some (n2', _) -> Some (Lt (n1, n2'), env)
        | None -> None)))
  | If (b, e1, e2), env -> (
    match b with
    | Bool b -> (
      match b with true -> Some (e1, env) | false -> Some (e2, env))
    | _ -> (
      match reduce b env with
      | Some (b', _) -> Some (If (b', e1, e2), env)
      | None -> None))
  | Product (n1, n2), env -> (
    match (n1, n2) with
    | _, _ -> (
      match reduce n1 env with
      | Some (n1', _) -> Some (Product (n1', n2), env)
      | None -> (
        match reduce n2 env with
        | Some (n2', _) -> Some (Product (n1, n2'), env)
        | None -> None)))
  | Fst n, env -> (
    match n with
    | Product (n1, _) -> Some (n1, env)
    | _ -> (
      match reduce n env with
      | Some (n', _) -> Some (Fst n', env)
      | None -> None))
  | Snd n, env -> (
    match n with
    | Product (_, n2) -> Some (n2, env)
    | _ -> (
      match reduce n env with
      | Some (n', _) -> Some (Snd n', env)
      | None -> None))
  | Let (x, e1, e2), env -> Some (e2, (x, e1) :: env)
  | _ -> None

let normalize (e : prog) : prog =
  let rec normalize' (e : prog) (env : env) : prog =
    match reduce e env with Some (e', env') -> normalize' e' env' | None -> e
  in
  normalize' e []
