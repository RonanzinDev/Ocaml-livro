type expr = 
  | Const of int (*Ex: Const 4 *)
  | Soma of expr * expr (*Ex: Soma (Const 4, Const 3) *)
  | Sub of expr * expr (*Ex: Sub (Const 4, Const 3) *)
  | Mult of expr * expr (*Ex: Mult (Const 4, Const 3) *)

(* 
  Exemplos
  1) Expressão: 4 + 3 * 2
     Arvore = Soma(Const 4, Mult (Const 3, Const 2))    

  2) Expressão: (4 + 3) * 2
      Arvore = Mult(Soma(Const 4, Const 3), Const 2)

  3) Expressão: (4	+	3)	*	2	+	5
    Árvore:	Soma (Mult (Soma (Const 4,	Const 3),Const 2),	Const	5)

     *)


type operacao	=	OpSoma	|	OpSub	|	OpMult

(* Instruções *)
(* Empilar = é mandar um numero para a stack, Oper = fazer uma operação com dois numeros da stack *)
type instrucao = Empilha of int | Oper of operacao

type programa = instrucao list

type pilha = int list

let operandos p =
  match p with
  | [] -> None
  | _ :: [] -> None
  (* numero 1, numero 2, resto *)
  | n1 :: n2 :: r -> Some ((n1, n2), r)

let oper o =
  match o with
  | OpSoma -> (+)
  | OpSub -> (-)
  | OpMult -> ( * )

(* p = Pilha, inst = Instrução *)
let exec_inst p inst =
  match inst with
  | Empilha n -> n :: p
  | Oper o -> 
      match operandos p with
      | None -> p
      | Some((n1, n2), r) ->
          let op = oper o in
          (op n1 n2) :: r

let rex exec_prog p =
  List.fold_left exec_inst [] p
(* Pretty priting *)
let rec print e =
  match e with
  | Const n -> string_of_int n
  | Soma(e1, e2) -> Printf.sprintf "(%s + %s)" (print e1) (print e2)
  | Sub(e1, e2) -> Printf.sprintf "(%s - %s)" (print e1) (print e2)
  | Mult(e1, e2) -> Printf.sprintf "(%s * %s)" (print e1) (print e2)

let input = Mult(Soma(Const 4, Const 3), Const 2)

let rec eval e =
  match e with
  | Const n -> n
  | Soma(e1, e2) -> eval e1 + eval e2
  | Sub(e1, e2) -> eval e1 - eval e2
  | Mult(e1, e2) -> eval e1 * eval e2

let result = eval input
let () =  print_int result