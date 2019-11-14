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
      |Empty ->""
      |Node(n)->"("^(test n.fg)^")"^(test n.fd) in test abr;;

(*TEST
print_string (parenth (construc [4;2;3;8;1;9;6;7;5]));;
print_string "\n";;
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
let replace expr mat rep = Str.global_replace mat rep expr;;

(*Parcours l'expression parenthese par et ajoute les pattern du type (int)int dans la table hash et renvoi hash *)
let rec parcours hash par =
  (*Printf.printf "%s \n" par ;*)
  if (Str.string_match (Str.regexp "^([0-9]+)[0-9]+$") par 0)
    then (Hashtbl.add hash (unique()) (Str.matched_string par); hash)
    else
      let _ = Str.search_forward (Str.regexp "([0-9]+)[0-9]+") par 0 in
        let tmp = Str.matched_string par in
          let key = unique() in
            Hashtbl.add hash key tmp;
            parcours hash (replace par (Str.regexp_string (Hashtbl.find hash key)) (string_of_int key));;

(*Retourne la table de hashage pour l'arbre abr*)
let getHash abr =
  let h = Hashtbl.create (getHauteur abr) in
    let uni=unique() in
      Hashtbl.add h uni "()";
      (*Printf.printf "%s \n" (parenth abr);*)
      let pa= replace (parenth abr) (Str.regexp "()") (string_of_int uni) in
        (*Printf.printf "%s \n" pa;*)
        parcours h pa;;


(*TEST
let x = getHash (construc [4;2;3;8;1;9;6;7;5]) in
  print_string (Hashtbl.find x 4);;
*)

(*Recupere le fils gauche/droit dans l'expression de la hashtable*)
let filsG h key = let v=(Hashtbl.find h key) in String.get v 1;;
let filsD h key = let v=(Hashtbl.find h key) in String.get v 3;; (*return char*)


(*Generateur de symbole et son reset*)
let reset_s, generate_symbol = let c = ref 0 in
  ( function () -> c:=0),
  ( function () -> c:=!c+1; "SYMB"^(string_of_int !c) );; (*ex: SYMB1*)


type 'a noeud = |Etq of int
                |Couple of {etq : int; liste : string list};;
(*TEST
let y = Couple {etq = 1;liste=["1";"2";"3"]} in
match y with
    |Couple(n) -> print_string (List.hd n.liste)
    | _ -> ();;
*)

type 'a comp = |Empty
               |Symbole of string
               |Node of {etq : 'a noeud list;fg : 'a comp;fd : 'a comp;id : int};;


let createNode etq fg fd id = Node {etq=etq;fg=fg;fd=fd;id=id};;

(*TEST
let x = (createNode [Etq 1;Etq 3] Empty Empty 2);;
*)
let addInNode node add = match node with
  |Node(n) -> createNode (add::n.etq) n.fg n.fd n.id
  |_ -> Empty;;

(*TEST
let y = addInNode x (Etq 5);;

match y with
  |Node(n) -> match n.etq with
        |h::t -> match h with
              |Etq(n) -> print_int n
              |_ -> ()
        | _ -> ()
  |_ -> ();;
*)

(*Hypothese: l'arbre n'est pas vide*)
let rec compTree  abr cle hash nodes symb = match abr with
  | Node(n) -> if n.fg = Empty && n.fd = Empty then (*On a une feuille*)
                  if symb = [] then mergeNodes nodes cle (Etq n.etq) Empty Empty
                  else mergeNodes nodes cle (Couple {etq=n.etq; liste=symb}) Empty Empty
              else
                    if  (Hashtbl.mem nodes cle) = true then
                      let tmp = Hashtbl.find nodes cle in match tmp with
                        |Node(n2) -> let ourSymb = generate_symbol in
                                        let fg = compTree n.fg (filsG hash cle) hash nodes (ourSymb::symb) in
                                          let fd = compTree n.fd (filsD hash cle) hash fg (ourSymb::n2.fd::[]) in
                                            mergeNodes fd cle (Couple{etq=n2.etq; liste=(ourSymb::symb)} Empty Empty
                        |_-> print_string "ERROR"
                    else
                        let fg= compTree n.fg (filsG hash cle) nodes symb in
                          let fd= compTree n.fd (filsD hash cle) fg symb in
                            mergeNodes fd cle (Etq cle)(*fils gauche*) (*fils droit*)


let compresser abr = let h = (getHash abr) and cle = (getHauteur abr) and symb = [] in
  let nodes = Hashtbl.create cle in match abr with
  | Empty -> print_string "L'arbre est vide"
  | Node(n) -> compTree abr cle h nodes symb ;; (*PAS FINI*)

(*node est soit un Etq of int ou un Couple
Cette fonction renvoie une hashtable avec la mise a jour des noeuds
Mais il faudrait qu'elle renvoie un couple symnole, hashtable*)
let mergeNodes h cle node fg fd =
  if (Hashtbl.mem h cle) = false then
    let newN = createNote [node] fg fd cle in Hashtbl.add h newN
  else
    let node2 = Hashtabl.find h cle in  let newN = addInNode node2 node in Hashtbl.add h newN;;

(*
let rec compTree abr cle hash n symb =
  if (Hashtbl.find n cle)=Empty then
    let tmp = match abr with
      |Node(n) -> match n.fg with
              |Empty(g) -> match n.fd with
                    |Empty(d) -> Hashtbl.add n cle (createNode (Etq n.etq) Empty Empty cle ); (Etq cle, n)
                    |Node(d) -> compTree
              |Node(g) -> (*compresser sur abr fg*)
      |Empty -> Empty

  else *)
