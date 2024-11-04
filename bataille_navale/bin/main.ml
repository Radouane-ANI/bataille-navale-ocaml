module Plateau = Bataille_navale.Plateau
module Bot = Bataille_navale.Bot
module Game = Bataille_navale.Game

(** Demande à l'utilisateur de placer des bateaux sur le plateau.
    @param p Le plateau sur lequel les bateaux doivent être placés. *)
let demander_placement_bateau (p : Plateau.plateau) : unit =
  let rec demander_coordonnees (longueur : int) =
    print_endline ("Placement du bateau de taille " ^ string_of_int longueur);
    print_endline "Entrez les coordonnées de départ du bateau (x y) :";
    let x = read_int () in
    let y = read_int () in
    print_endline "Le bateau est-il horizontal ? (o/n) :";
    let horizontal = read_line () = "o" in
    if Plateau.placer_bateau_valide p y x longueur horizontal then (
      print_endline "Bateau placé avec succès !";
      Plateau.afficher_plateau p)
    else (
      print_endline "Placement invalide. Veuillez réessayer.";
      demander_coordonnees longueur)
  in
  List.iter demander_coordonnees Plateau.flotte_standard

(* méthode qui appelle la commande clear sur le terminal grâce au module Sys*)
let clear() : unit = ignore (Sys.command "clear")

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
let rec boucle_jeu (gs : Game.game_state) (gv : Game.game_view) =
    print_endline "Coordonnées du prochain tir : ";
    let tir_x = read_line() in 
    let tir_y = read_line() in
    clear();
    (* initialise une variable du plateau de l'adversaire afin de l'afficher plus tard *)
    let plateau_ennemi = Game.plateau_a_attaquer gs in
    (* ligne qui tire sur une case sur le plateau de l'adversaire avec les coordonnées recupérés avec read_line()*)
    Plateau.tir plateau_ennemi (int_of_string tir_y, int_of_string tir_x);
    (* affiche le plateau de l'adversaire après le tir avec les nouveaux états *)
    Plateau.afficher_plateau_in_game plateau_ennemi;
    (* méthode qui termine le programme si la partie est terminée ou continue sinon*)
    if Plateau.endgame plateau_ennemi then (
      print_endline "Fin du jeu";
      exit 0
    ) else (
      freeze 3.;
      clear();
      match gs.current_player with
      | Player1 -> 
          print_endline "Plateau du joueur 1, au tour du joueur 2 de jouer.";
          Plateau.afficher_plateau_in_game gs.board_p1; 
          gs.current_player <- Player2
      | Player2 -> 
          print_endline "Plateau du joueur 2, au tour du joueur 1 de jouer.";
          Plateau.afficher_plateau_in_game gs.board_p2; 
          gs.current_player <- Player1;
      ;
      (* appel recursif de la boucle principale après avoir changer l'état du current_player de game_state dans Game*)
      boucle_jeu gs gv 
    )
  
  
let () =
  Random.self_init ();
  let plateau_1 = Plateau.init_plateau() in
  let plateau_2 = Plateau.init_plateau() in
  (* let mode = init_mode() in
    match mode with 
  | true -> init_mode_playervsbot plateau_1 plateau_2
  | false -> init_mode_playervsplayer plateau_1 plateau_2;
  ; *)
  init_mode_playervsplayer plateau_1 plateau_2;
  print_endline "Bateaux placés, place au jeu !";
  freeze 5.;
  clear();
  let new_game = Game.init_game plateau_1 plateau_2 in
  let game_v = Game.view new_game new_game.current_player in
  print_endline "Plateau du joueur 2, au tour du joueur 1 de jouer.";
  Plateau.afficher_plateau_in_game plateau_1;
  boucle_jeu new_game game_v
  
  
