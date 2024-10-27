module Plateau = Bataille_navale.Plateau

let demander_placement_bateau (p : Plateau.plateau) (longueur : int) : unit =
  let rec demander_coordonnees () =
    print_endline "Entrez les coordonnées de départ du bateau (x y) :";
    let x = read_int () in
    let y = read_int () in
    print_endline "Le bateau est-il horizontal ? (o/n) :";
    let horizontal = read_line () = "o" in
    if Plateau.placer_bateau_valide p x y longueur horizontal then
      print_endline "Bateau placé avec succès !"
    else (
      print_endline "Placement invalide. Veuillez réessayer.";
      demander_coordonnees ())
  in
  demander_coordonnees ()

let () =
  let plateau = Plateau.init_plateau () in
  demander_placement_bateau plateau 5
