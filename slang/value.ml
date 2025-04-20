type 'f t =
  | Ref of int
  | Int of int
  | Bool of bool
  | Unit
  | Pair of 'f t * 'f t
  | Inl of 'f t
  | Inr of 'f t
  | Fun of 'f

let rec pp pp_fun ppf = function
  | Ref a -> Format.fprintf ppf "Ref(%d)" a
  | Bool b -> Format.fprintf ppf "%b" b
  | Int n -> Format.fprintf ppf "%d" n
  | Unit -> Format.fprintf ppf "Unit"
  | Pair (v1, v2) ->
      Format.fprintf ppf "(@[%a,@ %a)@]" (pp pp_fun) v1 (pp pp_fun) v2
  | Inl v -> Format.fprintf ppf "Inl(%a)" (pp pp_fun) v
  | Inr v -> Format.fprintf ppf "Inr(%a)" (pp pp_fun) v
  | Fun f -> pp_fun ppf f
