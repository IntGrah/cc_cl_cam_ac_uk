
let add(p : int * int) : int =
    let h(z : int) : int = z + (snd p)
    in
       h(fst p)
in
  add(1, add(88, 12))
