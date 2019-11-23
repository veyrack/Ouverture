(*Remove l'element e de la liste l*)
let remove e l = List.filter (fun x -> x != e) l;;

(*Print la liste*)
let rec print_list = function
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l;;

(*Rempli la liste p avec les elements de la liste l tires aleatoirement*)
let extraction_alea l p = let size = List.length l in
    let r = (int_of_float (Unix.time())) mod size in
      let tmp = List.nth l r in
        ((remove tmp l) , (tmp :: p));;

(*TEST
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

(*TEST
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

(*TYPE du noeud d'un arbre compressé*)
type 'a noeud = |Etq of int
                |Couple of {etq : int; liste : string list};;

(*TYPE de l'arbre compressé*)
type 'a comp = |Empty
               |Symbole of string
               |NodeC of {etq : 'a noeud list;fg : 'a comp;fd : 'a comp;id : int};;
(*TEST
let y = Couple {etq = 1;liste=["1";"2";"3"]} in
match y with
    |Couple(n) -> print_string (List.hd n.liste)
    | _ -> ();;
*)

(*TEST
let x = getHash (construc [4;2;3;8;1;9;6;7;5]) in
  print_string (Hashtbl.find x 4);;
*)

(*Recupere le fils gauche/droit dans l'expression de la hashtable, rend un int*)
let filsG h key = let v=(Hashtbl.find h key) in int_of_string (String.make 1 (String.get v 1));;
let filsD h key = let v=(Hashtbl.find h key) in int_of_string (String.make 1 (String.get v 3));;


(*Generateur de symbole et son reset*)
let reset_s, generate_symbol = let c = ref 0 in
  ( function () -> c:=0),
  ( function () -> c:=!c+1; "SYMB"^(string_of_int !c) );; (*ex: SYMB1*)

(*TEST
let test = generate_symbol() in print_string test; let test2 = generate_symbol() in print_string test2;;
*)


(*Creer un noeud avec les parametres donnés*)
let createNode etq fg fd id = NodeC {etq=etq;fg=fg;fd=fd;id=id};;

(*TEST
let x = (createNode [Etq 1;Etq 3] Empty Empty 2);;
*)

(*Ajoute add dans l'etiquette AU DEBUT du noeud node *)
let addInNode node add = match node with
  |NodeC(n) -> createNode (add::n.etq) n.fg n.fd n.id
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

(*
- h est la hashtable contenant les noeuds
- cle est le numero de la structure
- node est soit une Etq of int ou un Couple (etq et symbole)
- fg/fd fils gauche/droit
Cette fonction renvoie renvoie un couple:(symbole), hashtable*)
let mergeNodes h cle node fg fd symb=
  if (Hashtbl.mem h cle) then
    let node2 = Hashtbl.find h cle in let newN = (addInNode node2 node) in (Hashtbl.add h cle newN) ;(symb,h)
  else
    let newN = (createNode [node] fg fd cle) in (Hashtbl.add h cle newN);(string_of_int cle ,h);;

(*Hypothese: l'arbre n'est pas vide*)
let rec compTree abr cle hash nodes lSymb = match abr with
  | Node(n) -> if n.fg = Empty && n.fd = Empty then (*On a une feuille*)
                  if lSymb = [] then
                    mergeNodes nodes cle (Etq n.etq) Empty Empty (string_of_int cle)
                  else
                    mergeNodes nodes cle (Couple {etq=n.etq; liste=lSymb}) Empty Empty (List.hd lSymb)
               else (*on a un noeud*)
                  if Hashtbl.mem nodes cle then
                    let tmp = Hashtbl.find nodes cle in match tmp with
                          |NodeC(n2) ->
                                          let fg = compTree n.fg (filsG hash cle) hash nodes lSymb in (*le fg existe*)
                                            let fd = compTree n.fd (filsD hash cle) hash (snd fg) (if (Hashtbl.mem nodes (filsD hash cle))
                                                                                                then ((match n2.fd with
                                                                                                                | Symbole(n) -> n
                                                                                                                | _ -> "ERROR")::lSymb)
                                                                                                else lSymb ) in
                                                                                                  mergeNodes (snd fd) cle (if Hashtbl.mem nodes cle
                                                                              then Couple{etq=n.etq; liste=(lSymb)}
                                                                              else (Etq n.etq)) (Symbole (fst fg)) (Symbole (fst fd)) (if Hashtbl.mem nodes cle
                                                                                                                  then List.hd lSymb
                                                                                                                  else string_of_int cle)

                          |_-> print_string "ERROR";("Err",nodes)

                  else (*le noeud n'existe pas*)
                    let fg= compTree n.fg (filsG hash cle) hash nodes lSymb in
                      let fd= compTree n.fd (filsD hash cle) hash (snd fg) (if Hashtbl.mem (snd fg) (filsG hash cle) then generate_symbol()::[] else lSymb ) in
                        mergeNodes (snd fd) cle (Etq n.etq) (Symbole (fst fg)) (Symbole (fst fd)) "" (*(if String.equal (fst fg) (fst fd) then (Symbol (fst fd)) else Symbol *)
  | _ -> print_string "ERROR";("Err",nodes);;






(* TEST PRINT*)
let printList f lst =
  let rec print_elements = function
    | [] -> ()
    | h::t -> f h; print_string ";"; print_elements t
  in
  print_string "{";
  print_elements lst;;


let print_noeud n = match n with
  |Etq(e) -> print_string "ETQ:"; print_int e
  |Couple(c) -> print_string "{ COUPLE:"; print_int c.etq; print_string "  Symboles:"; printList print_string c.liste; print_string "} } \n"
  |_ -> print_string "ERROR NOEUD";;



let rec print_compTree abr = match abr with
 | Empty -> print_string "Empty"
 | NodeC(n) -> printList print_noeud n.etq;
    print_string " fg:";
    print_compTree n.fg;
    print_string " fd:";
    print_compTree n.fd;
    print_string " id: ";
    print_int n.id;
    print_string " }  "
 | Symbole(s) -> print_string "SYMB: "; print_string s ;;

 (*TEST*)
(*let x = construc [4;2;3;8;1;9;6;7;5] in
  let y = getHash x in
    let z = compTree x 4 y (Hashtbl.create 4) [] in
      let nodes = snd z in print_compTree (Hashtbl.find nodes 4); print_string "\n";print_compTree (Hashtbl.find nodes 3); print_string "\n"; print_compTree (Hashtbl.find nodes 2); print_string "\n"; print_compTree(Hashtbl.find nodes 1);;
*)
(*
let compresser abr = let h = (getHash abr) and cle = (getHauteur abr) and symb = [] in
  let nodes = Hashtbl.create cle in match abr with
  | Empty -> print_string "L'arbre est vide"
  | NodeC(n) -> compTree abr cle h nodes symb ;; (*PAS FINI*)*)




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

let rec checkList l e = match l with
| h::t -> match h with
  | Etq(n) -> if n=e then h else checkList t e
  | Couple(n) -> if e=n.etq then h else checkList t e
  | _ -> Etq 0
| _ -> Etq 0;;

let rec search compT e = match compT with
  | Empty -> Empty
  | NodeC(n) -> let tmp=(checkList n.etq e) in
                            if tmp = Etq 0
                              then let tmp2 =(search n.fg e) in
                                if tmp2 = Empty
                                  then search n.fd e
                                  else tmp2
                              else Empty
  | _ -> Empty;;

let x = construc [4;2;3;8;1;9;6;7;5] in
  let y = getHash x in
    let z = compTree x 4 y (Hashtbl.create 4) [] in
      search z 1;;
