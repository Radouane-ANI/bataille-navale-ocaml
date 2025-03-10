module Game = Bataille_navale.Game
module Plateau = Bataille_navale.Plateau
open Game
open OUnit2

(* Tests pour l'initialisation du jeu avec des flottes complètes et incomplètes *)

let test_init_game_with_complete_fleets _ =
  let board_p1 = Plateau.init_plateau () in
  let board_p2 = Plateau.init_plateau () in
  (* Remplit les deux plateaux avec une flotte complète *)
  List.iteri
    (fun i taille ->
      ignore (Plateau.placer_bateau_valide board_p1 i i taille true))
    Plateau.flotte_standard;
  List.iteri
    (fun i taille ->
      ignore (Plateau.placer_bateau_valide board_p2 i i taille true))
    Plateau.flotte_standard;

  (* Teste l'initialisation avec des flottes complètes *)
  match init_game board_p1 board_p2 with
  | Next _ ->
      assert_bool "Board P1 complet" (Plateau.flotte_complete board_p1);
      assert_bool "Board P2 complet" (Plateau.flotte_complete board_p2)
  | Error _ ->
      assert_failure "Initialisation échouée malgré des flottes complètes"
  | _ -> assert_failure "Résultat inattendu lors de l'initialisation du jeu"

let test_init_game_with_incomplete_fleet _ =
  let board_p1 = Plateau.init_plateau () in
  let board_p2 = Plateau.init_plateau () in
  (* Place un bateau sur board_p1 pour qu'il soit incomplet *)
  ignore (Plateau.placer_bateau_valide board_p1 0 0 3 true);
  List.iteri
    (fun i taille ->
      ignore (Plateau.placer_bateau_valide board_p2 i i taille true))
    Plateau.flotte_standard;

  (* Teste l'initialisation avec une flotte incomplète sur board_p1 *)
  match init_game board_p1 board_p2 with
  | Error Invalid_board -> ()
  | _ ->
      assert_failure
        "Initialisation réussie malgré une flotte incomplète sur board_p1"

(* Test pour vérifier les erreurs sur deux flottes incomplètes *)
let test_init_game_with_both_incomplete_fleets _ =
  let board_p1 = Plateau.init_plateau () in
  let board_p2 = Plateau.init_plateau () in
  (* Place des bateaux de manière incomplète sur les deux plateaux *)
  ignore (Plateau.placer_bateau_valide board_p1 0 0 3 true);
  ignore (Plateau.placer_bateau_valide board_p2 1 1 2 false);

  (* Teste l'initialisation avec les deux flottes incomplètes *)
  match init_game board_p1 board_p2 with
  | Error Invalid_board -> ()
  | _ ->
      assert_failure
        "Initialisation réussie malgré des flottes incomplètes sur les deux \
         plateaux"

(* Test pour la fonction act *)
let test_act_invalid_turn _ =
  let board_p1 = Plateau.init_plateau () in
  let board_p2 = Plateau.init_plateau () in
  List.iteri
    (fun i taille ->
      ignore (Plateau.placer_bateau_valide board_p1 i i taille true))
    Plateau.flotte_standard;
  List.iteri
    (fun i taille ->
      ignore (Plateau.placer_bateau_valide board_p2 i i taille true))
    Plateau.flotte_standard;
  match init_game board_p1 board_p2 with
  | Next game_state ->
      (* Player2 essaie de jouer en premier, ce qui devrait échouer *)
      assert_equal (act Player2 (0, 0) game_state) (Error Not_Player_Turn)
  | _ ->
      assert_failure "Echec de l'initialisation pour le test act_invalid_turn"

let test_act_position_out_of_bounds _ =
  let board_p1 = Plateau.init_plateau () in
  let board_p2 = Plateau.init_plateau () in
  List.iteri
    (fun i taille ->
      ignore (Plateau.placer_bateau_valide board_p1 i i taille true))
    Plateau.flotte_standard;
  List.iteri
    (fun i taille ->
      ignore (Plateau.placer_bateau_valide board_p2 i i taille true))
    Plateau.flotte_standard;
  match init_game board_p1 board_p2 with
  | Next game_state ->
      (* Player1 essaie de jouer hors du plateau *)
      assert_equal
        (act Player1 (10, 0) game_state)
        (Error Position_out_of_bounds)
  | _ ->
      assert_failure
        "Echec de l'initialisation pour le test act_position_out_of_bounds"

let test_act_valid_turn _ =
  let board_p1 = Plateau.init_plateau () in
  let board_p2 = Plateau.init_plateau () in

  List.iteri
    (fun i taille -> ignore(Plateau.placer_bateau_valide board_p1 i i taille true))
    Plateau.flotte_standard;

  List.iteri
    (fun i taille -> ignore(Plateau.placer_bateau_valide board_p2 i i taille true))
    Plateau.flotte_standard;
  match init_game board_p1 board_p2 with
  | Next game_state -> (
      (* Player1 joue un coup valide *)
      match act Player1 (0, 0) game_state with
      | Next _ -> assert_bool "le test a reussi" true
      | _ ->
          assert_failure
            "Le coup valide de Player1 n'a pas abouti à un changement de joueur")
      | _ ->
          assert_failure "Echec de l'initialisation pour le test act_valid_turn"

(* Test pour la branche act où Player2 joue un tour valide *)
let test_act_player2_valid_turn _ =
  let board_p1 = Plateau.init_plateau () in
  let board_p2 = Plateau.init_plateau () in
  List.iteri
    (fun i taille -> ignore (Plateau.placer_bateau_valide board_p1 i i taille true))
    Plateau.flotte_standard;
  List.iteri
    (fun i taille -> ignore (Plateau.placer_bateau_valide board_p2 i i taille true))
    Plateau.flotte_standard;
  match init_game board_p1 board_p2 with
  | Next game_state -> (
      (* Player1 joue un coup valide *)
      match act Player1 (0, 0) game_state with
      | Next game_state' -> (
          (* Maintenant, Player2 peut jouer un tour valide *)
          match act Player2 (0, 0) game_state' with
          | Next _ -> assert_bool "Tour valide de Player2 réussi" true
          | _ -> assert_failure "Le tour valide de Player2 n'a pas abouti"
        )
      | _ -> assert_failure "Le coup de Player1 a échoué pour test_act_player2_valid_turn"
    )
  | _ -> assert_failure "Echec de l'initialisation pour le test_act_player2_valid_turn"

(** test supplementaire de Rayan Belhassen **)     

(* Test pour un tir sur une case déjà attaquée *)
let test_act_already_attacked_position _ =
  let board_p1 = Plateau.init_plateau () in
  let board_p2 = Plateau.init_plateau () in
  List.iteri
    (fun i taille -> ignore (Plateau.placer_bateau_valide board_p1 i i taille true))
    Plateau.flotte_standard;
  List.iteri
    (fun i taille -> ignore (Plateau.placer_bateau_valide board_p2 i i taille true))
    Plateau.flotte_standard;
  match init_game board_p1 board_p2 with
  | Next game_state -> (
      match act Player1 (0, 0) game_state with
      | Next game_state' -> (
          match act Player1 (0, 0) game_state' with
          | Next _ -> assert_bool "Tir répété accepté sans changer l'état" true
          | _ -> assert_bool "Pas d'erreur pour tir répété" true)
      | _ -> assert_failure "Le coup initial de Player1 n'a pas réussi")
  | _ -> assert_failure "Échec de l'initialisation pour le test act_already_attacked_position"


(* Test pour l'initialisation avec un plateau vide *)
let test_init_game_with_empty_boards _ =
  let board_p1 = Plateau.init_plateau () in
  let board_p2 = Plateau.init_plateau () in
  match init_game board_p1 board_p2 with
  | Error Invalid_board -> ()
  | _ ->
      assert_failure "Initialisation réussie malgré des plateaux vides"


(* Regroupement de tous les tests *)
let suite =
  "Test Game State Bataille Navale"
  >::: [
         "Initialisation du jeu avec flottes complètes" >:: test_init_game_with_complete_fleets;
         "Initialisation du jeu avec flotte incomplète" >:: test_init_game_with_incomplete_fleet;
         "Initialisation du jeu avec deux flottes incomplètes" >:: test_init_game_with_both_incomplete_fleets;
         "Act Not Player Turn" >:: test_act_invalid_turn;
         "Act Position Out Of Bounds" >:: test_act_position_out_of_bounds;
         "Act Valid Turn" >:: test_act_valid_turn;
        (* test suplementaire*)
        "Act Already Attacked Position" >:: test_act_already_attacked_position;
        "Initialisation du jeu avec plateaux vides" >:: test_init_game_with_empty_boards;
         "Act Player2 Valid Turn" >:: test_act_player2_valid_turn;
       ]

(*Pour réaliser les tests de l'affichage, il faut réaliser des fichiers .expected qui testera les fonctions d'affichage display et view pour les deux joueurs*)

(* Lancement des tests *)
let run = run_test_tt_main suite 
