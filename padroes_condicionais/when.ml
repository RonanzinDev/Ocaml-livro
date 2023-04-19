type dia = Quente | Frio | Agradavel

let classifica temps =
  match temps with
  | (_, max) when max > 30.0 -> Quente
  | (min, _) when min < 15.0 -> Frio
  | _ -> Agradavel

type naipe = Copas | Espadas | Ouro | Paus

let vermelha n =
  match n with
  | Copas | Ouro -> true
  | Espadas | Paus -> false


(*as-pattern*)
let rec ultimo l =
  match l with
  | [] -> 0
  | x :: [] -> x
  | _ :: resto -> ultimo resto

let min_max_lista l =
  match List.sort compare l with
  | [] -> (0,0)
  | min :: resto as l_ord-> (min, ultimo l_ord)