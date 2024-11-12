type bateau = (int * int) list
(** Représente un bateau comme une liste de coordonnées (x, y) sur le plateau *)

(** Représente l'état d'une case sur le plateau *)
type case =
  | Vide  (** La case est vide *)
  | Bateau of bateau  (** La case contient un bateau spécifique *)
  | Touche  (** La case a été touchée par un tir *)
  | Rate  (** La case a été visée par un tir mais elle est vide *)
  | Coule  (** La case contient un bateau qui a coule *)

type plateau = {
  grille : case array array;  (** Grille représentant les cases du plateau *)
  mutable ships : bateau list;  (** Liste des bateaux placés sur le plateau *)
}
(** Représente un plateau de jeu avec une grille et la liste de bateaux placés *)

(** Plateau vide de 10x10 cases, initialisé avec des cases vides *)
let plateau_vide = Array.init 10 (fun _ -> Array.init 10 (fun _ -> Vide))

(** La flotte standard représente la taille de chaque bateau requis pour le jeu *)
let flotte_standard = [ 2 ]

(** Initialise un nouveau plateau vide, avec une grille vide et sans bateaux placés *)
let init_plateau () : plateau = { grille = Array.init 10 (fun _ -> Array.init 10 (fun _ -> Vide)); ships = [] }

(** Vérifie si l'emplacement d'un bateau est valide sur le plateau donné.
    @param p Le plateau de jeu
    @param bateau Le bateau à vérifier (liste de coordonnées)
    @return true si toutes les cases du bateau sont dans les limites de la grille et vides, false sinon *)
let emplacement_valide (p : plateau) (bateau : bateau) : bool =
  List.for_all
    (fun (x, y) ->
      x >= 0
      && x < Array.length p.grille
      && y >= 0
      && y < Array.length p.grille.(0)
      && match p.grille.(x).(y) with Vide -> true | _ -> false)
    bateau

(** Génère un bateau de taille spécifiée, à partir des coordonnées de départ et dans une direction donnée.
    @param x Coordonnée x de départ
    @param y Coordonnée y de départ
    @param taille La taille du bateau
    @param horizontal true pour une orientation horizontale, false pour verticale
    @return Le bateau généré sous forme de liste de coordonnées *)
let generer_bateau (x : int) (y : int) (taille : int) (horizontal : bool) :
    bateau =
  List.init taille (fun i -> if horizontal then (x, y + i) else (x + i, y))

(** Place un bateau sur le plateau si son emplacement est valide.
    @param p Le plateau de jeu
    @param x Coordonnée x de départ
    @param y Coordonnée y de départ
    @param taille La taille du bateau
    @param horizontal true pour une orientation horizontale, false pour verticale
    @return true si le placement est réussi, false sinon *)
let placer_bateau_valide (p : plateau) (x : int) (y : int) (taille : int)
    (horizontal : bool) : bool =
  let bateau = generer_bateau x y taille horizontal in
  if emplacement_valide p bateau then (
    List.iter (fun (px, py) -> p.grille.(px).(py) <- Bateau bateau) bateau;
    p.ships <- bateau :: p.ships;
    true)
  else false

(** Vérifie si la flotte placée sur le plateau correspond exactement à la flotte standard
    @param p Le plateau de jeu
    @return true si tous les bateaux de la flotte standard sont placés, false sinon *)
let flotte_complete (p : plateau) : bool =
  let tailles_placees = List.map List.length p.ships in
  List.sort compare tailles_placees = List.sort compare flotte_standard

(** Génère une version cachée du plateau en masquant les cases où se trouvent les bateaux.
    Les cases marquées "Touche" ou "Rate" restent visibles, tandis que les cases avec des bateaux sont affichées comme "Vide".
    @param p Le plateau de jeu
    @return Une nouvelle grille où toutes les cases de bateaux sont masquées *)
let obtenir_plateau_cache (p : plateau) : case array array =
  Array.map
    (fun t ->
      Array.map (fun c -> match c with Bateau _ -> Vide | etats -> etats) t)
    p.grille

(* marque les case du plateau ou se trouve le bateau comme coule*)
let marque_coule (bateau : bateau) (p : plateau) : unit =
  List.iter (fun (px, py) -> p.grille.(px).(py) <- Coule) bateau

(* méthode qui mets à jour p.ships avec la méthode marque_coule quand un bateau est touché *)
let bateau_touche (p : plateau) (b : bateau) : unit =
  let est_coule =
    List.fold_left (fun acc (x, y) -> acc &&p.grille.(x).(y) = Touche) true b
  in
  if est_coule then (
    marque_coule b p;
    p.ships <- List.filter (( <> ) b) p.ships)
  else ()

(* méthode qui tire une frappe sur une case, avec une phrase qui s'affiche pour chaque cas *)
let tir (p : plateau) ((x, y) : int * int) : unit =
  match p.grille.(x).(y) with
  | Bateau b ->
      print_endline "Bateau touché !";
      p.grille.(x).(y) <- Touche;
      bateau_touche p b
  | Vide ->
      print_endline "Aucune bateau atteint";
      p.grille.(x).(y) <- Rate
  | Touche -> print_endline "Bateau déjà touché à cette position"
  | Rate -> print_endline "Position déjà frappé"
  | Coule -> print_endline "Bateau déjà coulé à cette position"

(* méthode qui vérifie si la partie est terminée (si un plateau se retrouve avec tout ses bateaux coulés)*)
let endgame (p : plateau) : bool = List.for_all (fun x -> x = []) p.ships

(* affiche une grille dans un format lisible par l'utilisateur
    @param g une grille du jeu*)
let afficher_grille (g : case array array) =
  let print_ligne1 =
    print_string "   ";
    for i = 0 to 9 do
      print_string (" " ^ string_of_int i ^ " ")
    done;
    print_newline ()
  in
  let affiche_ligne i ligne =
    print_string (" " ^ string_of_int i ^ " ");
    Array.iter
      (function
        | Touche -> print_string "\027[31m X \027[0m"
        | Bateau _ -> print_string "\027[32m b \027[0m"
        | Coule -> print_string "\027[31m B \027[0m"
        | Rate -> print_string "\027[33m O \027[0m"
        | Vide -> print_string "\027[34m ~ \027[0m")
      ligne;
    print_endline ""
  in
  print_ligne1;
  Array.iteri affiche_ligne g;
  print_endline ""

(** affiche le plateau dans un format lisible par l'utilisateur
    @param p Le plateau de jeu*)
let afficher_plateau (p : plateau) : unit = afficher_grille p.grille
