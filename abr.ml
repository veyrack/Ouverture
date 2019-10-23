(*Remove l'element e de la liste l*)
let remove e l = List.filter (fun x -> x != e) l;;

(*Print la liste*)
let rec print_list = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l;;

(*Rempli la liste p avec les elements de la liste l tires aleatoirement*)
let rec extraction_alea l p = let size = List.length l in
  if size = 0
    then p
  else
    let r = (int_of_float (Unix.time())) mod size in
      let tmp = List.nth l r in
        extraction_alea  (remove tmp l) (tmp :: p);;

(*TEST
let x = [1;5;6;2;3] in
  print_list (extraction_alea x []);
*)

(*Genere une liste triee d'entier de n a m*)
let rec interval n m = if n = m
    then [m]
    else n :: interval (n+1) m;;

(*Genere une liste triee d'enter de 1 a n puis
utilise extraction_alea avec cette liste et une liste vide*)
let gen_permutation n = let l =interval 1 n in
  let p = [] in
    extraction_alea l p;;

(*TEST
print_list (gen_permutation 4);;
*)

(*Definition du type 'a abr*)
type 'a abr = |Empty
              |Node of {etq : 'a ;fg : 'a abr;fd : 'a abr};;

(*TEST
let x = Node {etq = 1;fg = Empty;fd = Empty} in
  match x with
  | Node(n) -> print_int n.etq
  | _ -> () ;;
*)

(*Affiche l'abr*)
let rec print_abr abr = match abr with
| Empty -> print_string "<Empty>"
| Node(n) -> print_string "<";
            print_int n.etq;
            print_string " fg:";
            print_abr n.fg ;
            print_string " fd:";
            print_abr n.fd;
            print_string ">";;

(*Insere l'element e dans l'arbre abr*)
let rec inserer e abr = match abr with
  | Empty -> Node {etq = e;fg = Empty;fd= Empty}
  | Node(n) -> if e<n.etq
    then Node {etq=n.etq;fg = (inserer e n.fg); fd = n.fd}
    else Node {etq=n.etq;fg = n.fg ; fd = (inserer e n.fd)};;

(*TEST
let x = Node {etq = 1;fg = Empty;fd = Empty} in
  print_abr (inserer 2 x) ;;
*)

(*Construit l'abr a partir de la liste l*)
let construc l = let empt = Empty in
  let rec aux l abr = if l = []
    then abr
    else aux (List.tl l) (inserer (List.hd l) abr) in aux l empt;;

(*TEST
print_abr (construc [4;2;3;8;1;9;6;7;5]);;
*)

(* Renvoi l'arbre sous forme parenthese*)
let parenth abr=
  let rec test tree = match tree with
      |Empty ->"()"
      |Node(n)->"("^(test n.fg)^")"^(test n.fd) in test abr;;

(*TEST
print_string (parenth (construc [4;2;3;8;1]));;
*)
(*Retourne la hauteur d'un arbre*)
let rec getHauteur abr = match abr with
  |Empty -> 0
  |Node(n) -> max (1+getHauteur n.fg) (1+getHauteur n.fd);;


(*TEST
print_int (getHauteur (construc [4;2;1;3;8;6;5;7;9]));;
*)

(*Retourne un entier unique incremente a chaque appel*)
let unique =
  let last = ref 0 in
  fun () -> incr last ; !last;;

(*Test
let one = unique ();;  (* 1 *)

let two = unique ();;  (* 2 *)

let three = unique ();;  (* 3 *)
print_int three;;
*)

(*Remplace dans l'expression parenthese P les mat par rep*)
let replace P mat rep =
;;


let getHash abr = let pa = parenth abr in
  let h = Hashtbl.create (getHauteur abr) in
    Hashtbl.add h unique() "()" in
    let rec parcours hash par = match par with
    | [] -> expr
    | _ -> expr2
