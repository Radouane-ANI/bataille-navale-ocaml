(* méthode qui force le joueur a choisir une valeur entre 0 et 9*)
let rec coord () : int =
  try 
    let input = read_line () in
    let n = int_of_string input in
    if n >= 0 && n < 10 then
      n
    else (
      print_endline "Seuls les nombres entre 0 et 9 sont acceptés.";
      coord ()
    )
  with 
  | Failure _ -> 
      print_endline "Entrée invalide. Veuillez entrer un entier.";
      coord ()
  | _ -> 
      print_endline "Erreur inattendue. Veuillez réessayer.";
      coord ()
 

(* méthode qui force le joueur a choisir "o" ou "n" pour l'orientation des bateaux *)
let rec orientation_bateau () : bool = 
  let rep = String.lowercase_ascii (String.trim (read_line ())) in
  match rep with 
  | "o" | "O" -> true
  | "n" | "N" -> false
  | _ -> 
      print_endline "Valeur non valide. Veuillez entrer 'o' pour horizontal ou 'n' pour vertical.";
      orientation_bateau ()
