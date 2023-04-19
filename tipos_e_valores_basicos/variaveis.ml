(*nao ficara disponivel no modulo todo*)
let x = 2 in x * x

(*Exeplo de uso do 'in'*)

let raiz_positiva a b c = 
  if (b *. b -. 4.0 *. a *.c) >= 0.0 then
    (-.b +. (sqrt (b *. b -. 4.0 *. a *. c))) /. (2.0 *. a)
  else
    infinity

(*bom uso do 'in'*)
let raiz_positiva2 a b c =
  let delta = b *. b -. 4.0 *. a *. c in
  if delta >= 0.0 then
    (-. b +. sqrt delta) /. 2.0 *. a
  else
  infinity

