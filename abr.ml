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

(*TEST*)
(*let x = [1;5;6;2;3] in
  print_list (extraction_alea x []);
*)

(*Genere une liste triee d'entier de n a m*)
let rec interval n m = if n = m
    then [m]
    else n :: interval (n+1) m;;

(*Genere une liste triee d'enter de 1 a n puis utilise extraction_alea avec cette liste et une liste vide*)
let gen_permutation n = let l =interval 1 n in
  let p = [] in
    extraction_alea l p;;

(*TEST*)
print_list (gen_permutation 4);;
