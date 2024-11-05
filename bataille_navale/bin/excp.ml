(* méthode qui force le joueur a choisir une valeur entre 0 et 9*)
let rec coord() : int =
  try 
    let n = read_int() in
    match n with 
    | _ when n >= 0 && n < 10 -> n
    | _ -> print_endline "Seuls les valeurs entre 0 et 9 sont acceptées."; coord()
  with 
  | Failure _ -> print_endline "Type incorrect, seuls les entiers sont acceptés."; coord()

(* méthode qui force le joueur a choisir "o" ou "n" pour l'orientation des bateaux *)
let rec orientation_bateau() : bool = 
  let rep = read_line() in
  match rep with 
  | "o" -> true
  | "n" -> false
  | _ -> print_endline "Valeur non valide. Le bateau est-il horizontal ? (o/n) :"; orientation_bateau()