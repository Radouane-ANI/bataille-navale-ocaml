module Plateau = Bataille_navale.Plateau
module Bot = Bataille_navale.Bot
module Game = Bataille_navale.Game

(** Demande à l'utilisateur de placer des bateaux sur le plateau.
    @param p Le plateau sur lequel les bateaux doivent être placés. *)
let demander_placement_bateau (p : Plateau.plateau) : unit =
  let rec demander_coordonnees (longueur : int) =
    print_endline ("Placement du bateau de taille " ^ string_of_int longueur);
    print_endline "Entrez les coordonnées de départ du bateau (x y) :";
    let x = print_string "x : "; Excp.coord() in
    let y = print_string "y : "; Excp.coord() in
    print_endline "Le bateau est-il horizontal ? (o/n) :";
    let horizontal = Excp.orientation_bateau() in
    if Plateau.placer_bateau_valide p y x longueur horizontal then (
      print_endline "Bateau placé avec succès !";
      Plateau.afficher_plateau p)
    else (
      print_endline "Placement invalide. Veuillez réessayer.";
      demander_coordonnees longueur)
  in
  List.iter demander_coordonnees Plateau.flotte_standard

(* méthode qui appelle la commande clear sur le terminal grâce au module Sys*)
let clear() : unit = 
  let clear =
    if Sys.os_type = "Win32" then "cls"
    else "clear"
  in ignore (Sys.command clear)

(* méthode pour geler le programme *)
let freeze(duree : float) : unit =
  let debut = Sys.time () in
  while Sys.time () -. debut < duree do
    ()
  done
  
(* méthode qui demande si l'utilisateur veut jouer contre l'ordinateur ou à deux joueurs*)
(* let rec init_mode() : bool =
  print_endline "Jouer contre l'ordinateur ou à deux joueurs ? (Bot : 1 - 1v1 : 2)";
  let rep = read_line () in 
  match rep with
  | "1" -> true    
  | "2" -> false
  | _ -> print_endline "Réponse invalide, réessayez."; init_mode() *)

(* méthode qui initialise le mode joueur contre ordinateur*)
(* let init_mode_playervsbot(p1 : Plateau.plateau) (p2 : Plateau.plateau) : unit = 
  demander_placement_bateau p1;
  Bot.placer_flotte_aleatoire p2 *)

(* méthode qui initialise le mode joueur 1 contre joueur 2*)
let init_mode_playervsplayer(p1 : Plateau.plateau) (p2 : Plateau.plateau) : unit = 
  demander_placement_bateau p1;
  print_endline "\nPlacement terminé, passez au joueur 2 (dans 5 secondes)";
  freeze 5.;
  clear();
  demander_placement_bateau p2

(* méthode de la boucle principale du jeu*)
let rec boucle_jeu (gs : Game.game_state) (pl : Game.player) : unit =
  (match pl with
  | Game.Player1 -> print_endline "Plateau du joueur 1, au tour du joueur 2 de jouer."
  | Game.Player2 -> print_endline "Plateau du joueur 2, au tour du joueur 1 de jouer.");
  Game.display (Game.view gs pl);
  print_endline "Coordonnées du prochain tir : ";
  let tir_x = print_string "x : "; Excp.coord() in
  let tir_y = print_string "y : "; Excp.coord() in
  let display_and_next new_gs next_player =
    Game.display (Game.view new_gs pl);
    freeze 3.;
    clear ();  
    boucle_jeu new_gs next_player
  in
  (* Processus pour gérer les différents résultats de `Game.act` *)
  let next_move next_player =
    match Game.act pl (tir_y, tir_x) gs with
    | Endgame _ -> print_endline "Fin du jeu"
    | Next new_gs ->
        display_and_next new_gs next_player
    | _ ->
        (* Cas où le tir n'entraîne pas de changement de joueur *)
        print_endline "Aucun changement d'état, rejouez.";
        display_and_next gs  pl
  in
  (* Déterminer le joueur suivant en fonction du joueur actuel *)
  match pl with
  | Game.Player1 -> next_move Game.Player2
  | Game.Player2 -> next_move Game.Player1
  
let () =
  Random.self_init ();
  let rec start () = 
    let plateau_1 = Plateau.init_plateau () in
    let plateau_2 = Plateau.init_plateau () in

    (* Uncomment this section if you want to initialize different modes
    let mode = init_mode () in
    match mode with 
    | true -> init_mode_playervsbot plateau_1 plateau_2
    | false -> init_mode_playervsplayer plateau_1 plateau_2;
    *)

    (* Initialize for player vs player mode directly *)
    init_mode_playervsplayer plateau_1 plateau_2;
    clear();
    print_endline "Bateaux placés, place au jeu !";

    let new_game = Game.init_game plateau_1 plateau_2 in
    match new_game with
    | Next gs -> boucle_jeu gs Game.Player1
    | _ -> start () (* Recursive call to restart the game if needed *)
  in
  start ()
