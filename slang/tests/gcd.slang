
let gcd(p : int * int) : int =
    let m : int = fst p in
       let  n : int = snd p in
          if m = n
          then m
          else if m < n
               then gcd(m, n - m)
               else gcd(m - n, n)
in
   let x : int = 10 in
      let  y : int = 2 in gcd(x, y)
