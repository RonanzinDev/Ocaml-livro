type 'a arvore = Folha | No of 'a arvore * 'a  * 'a arvore

let a = No(Folha, 7, No(Folha, 9, Folha))

let rec soma_arvore a =
  match a with
  | Folha -> 0
  | No(a1, n, a2) -> soma_arvore a1 + n + soma_arvore a2


let rec busca a v =
  match a with
  | Folha -> None
  | No(a1, n, a2) when n = v -> Some a
  | No(a1, n, _) when v < n -> busca a1 v
  | No (_, n, a2) -> busca a2 v


let a2 = 	No	(No	(Folha,	17,	Folha),	21,	No	(Folha,	42,	Folha));;
let busca1 = busca a2 111
let r = Option.get busca1

