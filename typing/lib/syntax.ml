type prog =
  | Bool of bool
  | Int of int
  | Unit
  | Add of prog * prog
  | Lt of prog * prog
  | If of prog * prog * prog
  | Product of prog * prog
  | Fst of prog
  | Snd of prog
