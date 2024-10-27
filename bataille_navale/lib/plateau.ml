type bateau = (int * int) list
type case = | Vide | Bateau of bateau | Touche | Rate
type plateau = {
  grille : case array array;  
  mutable ships : bateau list;     
}

let plateau_vide = Array.init 10  (fun _ -> Array.init 10 (fun _ -> Vide))

let init_plateau () : plateau = 
  { grille = plateau_vide; ships = [] }

let emplacement_valide (p : plateau) (bateau : bateau) : bool = 
  List.for_all (fun (x, y) ->
    x >= 0 && x < Array.length p.grille &&  
    y >= 0 && y < Array.length p.grille.(0) &&
    match p.grille.(x).(y) with
    | Vide -> true  
    | _ -> false
  ) bateau

let generer_bateau (x : int) (y : int) (taille : int) (horizontal : bool) : bateau =
  List.init taille (fun i -> if horizontal then (x, y+i) else (x+i, y))
  
let placer_bateau_valide (p : plateau) (x : int) (y : int) (taille : int) (horizontal : bool) : bool =
  let bateau = generer_bateau x y taille horizontal in
  if emplacement_valide p bateau then 
(List.iter (fun (px,py) -> p.grille.(px).(py) <- Bateau (bateau)) bateau; 
  p.ships <- bateau :: p.ships;
  true) 
else false

