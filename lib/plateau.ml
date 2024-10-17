open Array
type bateau = | Petit | Moyen | Grand
type case = | Vide | Bateau of bateau | Touche | Rate
type plateau = case array array

let plateau_vide = Array.init 10  (fun _ -> Array.init 10 (fun _ -> Vide))
let () = print_int 2