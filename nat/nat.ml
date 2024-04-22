type nut = Zero | Succ of nut

let add n m =
  let rec add' n m = match n with Zero -> m | Succ n' -> Succ (add' n' m) in
  add' n m

let rec even n =
  match n with Zero -> true | Succ (Succ n') -> even n' | Succ n' -> false

let pred n =
  match Some n with
  | None -> None
  | Some Zero -> None
  | Some (Succ n') -> Some n'

let half n =
  let rec half' n =
    match n with
    | Zero -> Zero
    | Succ Zero -> Zero
    | Succ (Succ n') -> Succ (half' n')
  in
  half' n

let half_opt n = if even n then Some (half n) else None

(* check functions using the following commands *)
(*
   add (Succ (Succ (Succ Zero))) (Succ (Succ Zero));;
   even (Succ (Succ (Succ Zero)));;
   even (Succ (Succ (Succ (Succ Zero))));;
   pred (Succ (Succ (Succ Zero)));;
   pred Zero;;
   half (Succ (Succ (Succ (Succ (Succ Zero)))));;
   half (Succ (Succ (Succ (Succ Zero))));;
   half_opt (Succ (Succ (Succ (Succ (Succ Zero)))));;
   half_opt (Succ (Succ (Succ (Succ Zero))));;
*)
