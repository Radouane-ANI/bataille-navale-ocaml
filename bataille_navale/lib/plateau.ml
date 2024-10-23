open Array
type bateau = (int * int) list
type case = | Vide | Bateau of bateau | Touche | Rate
type plateau = {
  grille : case array array;  
  ships : bateau list;     
}

let plateau_vide = Array.init 10  (fun _ -> Array.init 10 (fun _ -> Vide))
let () = print_int 2