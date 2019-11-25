ABR-compress-map(*Remove l'element e de la liste l*)
let remove e l = List.filter (fun x -> x != e) l;;

(*Print la liste pour des entiers*)
let rec print_list = function
    [] -> ()
  | e::l -> print_int e ; print_string " " ; print_list l;;

(*Rempli la liste p avec les elements de la liste l tires aleatoirement*)
let extraction_alea l p = let size = List.length l in
  let r = (int_of_float (Unix.time())) mod size in
  let tmp = List.nth l r in
  ((remove tmp l) , (tmp :: p));;

(*TEST de extraction_alea
  let x = [1;2;3;4;5] in
  let tmp= extraction_alea x [] in
    print_list (fst tmp);
    print_string "and ";
    print_list (snd tmp);;
*)


(*Genere une liste triee d'entier de n a m*)
let rec interval n m = if n = m
  then [m]
  else n :: interval (n+1) m;;

(*Genere une liste triee d'enter de 1 a n puis
  utilise extraction_alea avec cette liste et une liste vide*)
let gen_permutation n = let l =interval 1 n in
  let p = [] in
  let rec tmp ll pp = if List.length ll = 0
    then pp
    else
      let ext=(extraction_alea ll pp) in
      tmp (fst ext) (snd ext) in
  tmp l p;;

(*TEST
  print_list (gen_permutation 5);;
*)


(*Definition du type 'a abr*)
type abr = |Empty
           |Node of {etq : int ;fg : abr;fd : abr};;


(*TEST du type abr
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

(*TEST de inserer
  let x = Node {etq = 1;fg = Empty;fd = Empty} in
  print_abr (inserer 2 x) ;;
*)

(*Construit l'abr a partir de la liste d'entier l*)
let construc l = let empt = Empty in
  let rec aux l abr = if l = []
    then abr
    else aux (List.tl l) (inserer (List.hd l) abr) in aux l empt;;

(*TEST
  print_abr (construc [4;2;3;8;1;9;6;7;5]);;
*)

(* Renvoi l'arbre sous forme de parentheses*)
let parenth abr=
  let rec test tree = match tree with
    |Empty ->""
    |Node(n)->"("^(test n.fg)^")"^(test n.fd) in test abr;;

(*TEST de parenth
  print_string (parenth (construc [4;2;3;8;1;9;6;7;5]));;
  print_string "\n";;
*)

(*Retourne la hauteur d'un arbre*)
let rec getHauteur abr = match abr with
  |Empty -> 0
  |Node(n) -> max (1+getHauteur n.fg) (1+getHauteur n.fd);;


(*TEST de getHauteur
  print_int (getHauteur (construc [4;2;1;3;8;6;5;7;9]));;
*)


(*TYPE du noeud d'un arbre compressé*)
type noeud = |Etq of int
             |Couple of {etq : int; liste : string list};;

(*TYPE de l'arbre compressé*)
type comp = |Empty
            |Symbole of string * comp ref
            |NodeC of {etq : noeud list;fg : comp ref ;fd : comp ref };;

(*TEST des types
  let y = Couple {etq = 1;liste=["1";"2";"3"]} in
  match y with
    |Couple(n) -> print_string (List.hd n.liste)
    | _ -> ();;
*)

(*Generateur de symbole et son reset*)
let reset_s, generate_symbol = let c = ref 0 in
  ( function () -> c:=0),
  ( function () -> c:=!c+1; "SYMB"^(string_of_int !c) );; (*ex: SYMB1*)

(*TEST de generate_symbol
  let test = generate_symbol() in print_string test; let test2 = generate_symbol() in print_string test2;;
*)


(*Creer un noeud avec les parametres donnés*)
let createNode etq fg fd= NodeC {etq=etq;fg=fg;fd=fd};;

(*TEST
  let x = (createNode [Etq 1;Etq 3] Empty Empty);;
*)

(*Ajoute add dans l'etiquette AU DEBUT du noeud node *)
let addInNode node add = match node with
  |NodeC(n) -> createNode (add::n.etq) n.fg n.fd
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



(*Cette fonction renvoie renvoie un couple:(string, hashtable) *)
let mergeNodes h_nodes mot node fg fd symb =
  if (Hashtbl.mem h_nodes mot) then
    let node2 = Hashtbl.find h_nodes mot in let newN = (addInNode !node2 node) in node2:= newN ;
    (symb,h_nodes)
  else
    let newN = (createNode [node] fg fd) in (Hashtbl.add h_nodes mot (ref newN));
    (mot ,h_nodes);;

(*Renvoi une ref de noeud, un symbole si s est un symbole et un NodeC sinon*)
let creerFils h_nodes s mot= if (Str.string_match (Str.regexp "^SYMB[0-9]+") s 0)
  then ref (Symbole (s, Hashtbl.find h_nodes mot))
  else Hashtbl.find h_nodes mot;;

(* Compresse le noeud d'un arbre
Hypothese: l'arbre n'est pas vide*)
let rec compTree  abr h_nodes lSymb = let mot = parenth abr in
  match abr with
  | Empty -> if (Hashtbl.mem h_nodes mot) then (mot, h_nodes) else begin Hashtbl.add h_nodes mot (ref Empty); (mot, h_nodes) end
  | Node(n) -> (   (*Notre ancien noeud*)
      if (n.fg = Empty && n.fd = Empty) (*ici on traite la feuille*)
      then mergeNodes h_nodes mot (if lSymb = [] then Etq n.etq else Couple {etq=n.etq; liste=lSymb}) (ref Empty) (ref Empty) (if lSymb=[] then "" else (List.hd lSymb))
      else if (Hashtbl.mem h_nodes mot) (*Noeud existe deja*)
      then let tmp = Hashtbl.find h_nodes mot in match !tmp with
        |NodeC(n2) ->  let _ = (match !(n2.fg) with
                                  |Empty -> compTree n.fg h_nodes lSymb
                                  |Symbole(s,h) -> compTree n.fg h_nodes (s::lSymb)
                                  |NodeC(n3) -> compTree n.fg h_nodes lSymb) in
                          let _ = (match !(n2.fd) with
                              |Empty -> compTree n.fd h_nodes lSymb
                              |Symbole(s,h) -> compTree n.fd h_nodes (s::lSymb)
                              |NodeC(n4) -> compTree n.fd h_nodes lSymb) in
                                mergeNodes h_nodes mot (Couple{etq=n.etq; liste=lSymb}) (ref Empty) (ref Empty) (if lSymb=[] then "" else (List.hd lSymb)) (*On n'a pas besoin de savoir les fg et fd *)
        |_ -> ("Err",h_nodes)
      else (*Noeud n'existe pas*)
        let fg = compTree n.fg h_nodes (if Hashtbl.mem h_nodes (parenth n.fg) then generate_symbol()::[] else lSymb ) in
        let fd = compTree n.fd h_nodes (if Hashtbl.mem h_nodes (parenth n.fd) then generate_symbol()::[] else lSymb ) in
        mergeNodes h_nodes mot (Etq n.etq) (creerFils h_nodes (fst fg) (parenth n.fg)) (creerFils h_nodes (fst fd) (parenth n.fd)) "" );;

(*Fonction (principale) qui compresse l'arbre*)
let ABR-compress-listes abr = let nodes = Hashtbl.create (getHauteur abr) in let c = compTree abr nodes [] in !(Hashtbl.find (snd c) (parenth abr)) ;;


(* Print une liste en appliquant f a la liste*)
let printList f lst =
  let rec print_elements = function
    | [] -> ()
    | h::t -> f h; print_string ";"; print_elements t
  in
  print_string "{";
  print_elements lst;;

(*Affiche un noeud*)
let print_noeud n = match n with
  |Etq(e) -> print_string "ETQ:"; print_int e
  |Couple(c) -> print_string "{ COUPLE:"; print_int c.etq; print_string "  Symboles:"; printList print_string c.liste; print_string "} } \n";;


(*Affiche un abre compresse*)
let rec print_compTree abr = match abr with
  | Empty -> print_string "Empty"
  | NodeC(n) -> printList print_noeud n.etq;
    print_string " fg:";
    print_compTree !(n.fg);
    print_string " fd:";
    print_compTree !(n.fd);
    print_string " }  "
  | Symbole(s,h) -> print_string "SYMB: "; print_string s(*; print_compTree !h*);;

(*TEST de l'affichage d'un arbre compressé*)
(*
let x = construc [1;2;3;4] in
  let y = Hashtbl.create 4 in
    let z = compTree x y [] in
      let nodes = snd z in print_compTree !(Hashtbl.find nodes (parenth x));;
*)

(*TEST de la fonction ABR-compress-listes
let x = construc [4;2;3;8;1;9;6;7;5] in
  let compress = ABR-compress-listes x in print_compTree compress;;
*)

(*Check si e est dans la liste l
renvoi 0 si oui,sinon -1 si e < à l'etiquette,1 sinon*)
let rec checkList l e = match l with
  | [] -> 2
  | h::t -> match h with
    | Etq(n) -> if n=e then 0 else if n<e then 1 else -1
    | Couple(n) -> if e=n.etq then 0 else checkList t e


(*Recherche a partir du noeud compT si e est contenue dans 'arbre'*)
let rec search compT e = match compT with
  | Empty -> false
  | NodeC(n) -> let tmp = checkList n.etq e in
    if tmp = 0
    then true
    else if tmp = 1
    then search !(n.fd) e
    else search !(n.fg) e
  | Symbole(n,c) -> search !c e;;



(*TEST DE RECHERCHE*)
(*
let a = construc [4;2;3;8;1;9;6;7;5] in
let b = Hashtbl.create 4 in
let c = compTree a b [] in
Printf.printf "%B\n" (search !(Hashtbl.find (snd c) (parenth a)) 10);;
*)


(*--------------------------------------- Question 2.7-----------------------------------------------------*)
(*TYPE de l'arbre compressé map*)
type comp_map = |EmptyM
                |SymboleM of string * comp_map ref
                |NodeM of {etq : (int,string list) Hashtbl.t ;fg : comp_map ref ;fd : comp_map ref }


(*map: notre Hashtbl de NodeM ; etq: int  ; lsymb: liste de symbole*)
let addInNodeMap map etq lsymb = match map with
  | NodeM(n) -> Hashtbl.add n.etq etq lsymb; NodeM {etq=n.etq;fg=n.fg;fd=n.fd}
  | _ -> EmptyM;;


let mergeNodesMap h_nodes mot fg fd lsymb etq =
  if (Hashtbl.mem h_nodes mot) then
    let node2 = Hashtbl.find h_nodes mot in let newN = (addInNodeMap !node2 etq lsymb ) in node2:= newN ;
    ((if lsymb=[] then "" else List.hd lsymb), h_nodes)
  else
    let tmp = Hashtbl.create 1 in Hashtbl.add tmp etq [];
    let newN = NodeM {etq=tmp;fg=fg;fd=fd} in (Hashtbl.add h_nodes mot (ref newN));
    (mot , h_nodes);;

let creerFilsMap h_nodes s mot = if (Str.string_match (Str.regexp "^SYMB[0-9]+") s 0)
  then ref (SymboleM (s,Hashtbl.find h_nodes mot))
  else Hashtbl.find h_nodes mot;;


let rec compTreeMap  abr h_nodes lSymb = let mot = parenth abr in
  match abr with
  | Empty -> if (Hashtbl.mem h_nodes mot) then (mot, h_nodes) else begin Hashtbl.add h_nodes mot (ref EmptyM); (mot, h_nodes) end
  | Node(n) -> (   (*Notre ancien noeud*)
      if (n.fg = Empty && n.fd = Empty) (*ici on traite la feuille*)
      then mergeNodesMap h_nodes mot (ref EmptyM) (ref EmptyM) lSymb n.etq
      else if (Hashtbl.mem h_nodes mot) (*Noeud existe deja*)
      then let tmp = Hashtbl.find h_nodes mot in match !tmp with
        |NodeM(n2) ->  let _ = (match !(n2.fg) with
            |EmptyM -> compTreeMap n.fg h_nodes lSymb
            |SymboleM(s,h) -> compTreeMap n.fg h_nodes (s::lSymb)
            |NodeM(n3) -> compTreeMap n.fg h_nodes lSymb) in
          let _ = (match !(n2.fd) with
              |EmptyM -> compTreeMap n.fd h_nodes lSymb
              |SymboleM(s,h) -> compTreeMap n.fd h_nodes (s::lSymb)
              |NodeM(n4) -> compTreeMap n.fd h_nodes lSymb) in
          mergeNodesMap h_nodes mot (ref EmptyM) (ref EmptyM) lSymb n.etq (*On n'a pas besoin de savoir les fg et fd *)
        |_ -> ("Err",h_nodes)
      else (*Noeud n'existe pas*)
        let fg = compTreeMap n.fg h_nodes (if Hashtbl.mem h_nodes (parenth n.fg) then generate_symbol()::[] else lSymb ) in
        let fd = compTreeMap n.fd h_nodes (if Hashtbl.mem h_nodes (parenth n.fd) then generate_symbol()::[] else lSymb ) in
        mergeNodesMap h_nodes mot (creerFilsMap h_nodes (fst fg) (parenth n.fg)) (creerFilsMap h_nodes (fst fd) (parenth n.fd)) [] n.etq);;

let ABR-compress-map abr = let nodes = Hashtbl.create (getHauteur abr) in let c = compTreeMap abr nodes [] in !(Hashtbl.find (snd c) (parenth abr)) ;;


(*TEST de compression map
  let x = construc [4;2;3;8;1;9;6;7;5] in
  let y = getHash x in
    let z = compTreeMap x 4 y (Hashtbl.create 4) [] in
      let nodes = snd z in print_compTreeMap !(Hashtbl.find nodes 4);;*)
(*
let x = construc [4;2;3;8;1;9;6;7;5] in
  let compress = ABR-compress-map x in print_compTreeMap compress;;
*)



let rec print_hash h = let f x y = print_int x;printList print_string y in
  begin
    print_string "ETQ: ";
    Hashtbl.iter f h;
  end;;

let rec print_compTreeMap abr = match abr with
  | EmptyM -> print_string "Empty"
  | NodeM(n) -> print_hash n.etq;
    print_string " fg:";
    print_compTreeMap !(n.fg);
    print_string " fd:";
    print_compTreeMap !(n.fd);
    print_string " }  "
  | SymboleM(s,h) -> print_string "SYMB: "; print_string s(*; print_compTree !h*);;

(*TEST AFFICHAGE DE LARBRE MAP*)
(*
let a = construc [4;2;3;8;1;9;6;7;5] in
  let b = Hashtbl.create 4  in
    let c = compTreeMap a b [] in
let nodes = snd c in print_compTreeMap !(Hashtbl.find nodes (parenth a));;
*)


(*Fonction de recherche dans un arbre compresse map*)
let rec searchMap compT e = match compT with
  | EmptyM -> false
  | NodeM(n) -> if (Hashtbl.mem n.etq e) then true
    else
    if not (searchMap !(n.fg) e)
    then searchMap !(n.fd) e
    else true
  | SymboleM(n,c) -> searchMap !c e;;


(*TEST DE RECHERCHE DANS MAP*)
(*
  let a = construc [4;2;3;8;1;9;6;7;5] in
    let b = Hashtbl.create 4 in
      let c = compTreeMap a b [] in
        Printf.printf "%B\n" (searchMap !(Hashtbl.find (snd c) (parenth a)) 2);;
*)
