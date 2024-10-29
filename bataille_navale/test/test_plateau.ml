module Plateau = Bataille_navale.Plateau
open Plateau
open OUnit2

(* Tests pour l'initialisation du plateau *)
let test_init_plateau _ =
  let grille = plateau_vide in
  assert_equal (Array.length grille) 10;
  (* Vérification de la taille de la grille *)
  assert_equal (Array.length grille.(0)) 10;
  (* On vérifie que toutes les cases sont marquées comme Vide *)
  Array.iter
    (fun row -> Array.iter (fun cell -> assert_equal cell Vide) row)
    grille

(* Test de vérification de l'emplacement valide *)
let test_emplacement_valide _ =
  let p = init_plateau () in
  let bateau = generer_bateau 0 0 3 true in
  let emplacement_possible = emplacement_valide p bateau in
  assert_bool "Emplacement possible pour un bateau valide" emplacement_possible;

  (* Tentative de placer un bateau hors limites *)
  let bateau_hors_limite = generer_bateau 8 8 4 true in
  let emplacement_impossible = not (emplacement_valide p bateau_hors_limite) in
  assert_bool "Emplacement impossible pour un bateau hors des limites"
    emplacement_impossible

(* Test du placement de bateau valide *)
let test_placer_bateau_valide _ =
  let p = init_plateau () in
  let success = placer_bateau_valide p 0 0 3 true in
  assert_bool "Placement valide d'un bateau" success;

  (* Tentative de placement d'un bateau qui se superpose *)
  let superpose = not (placer_bateau_valide p 0 0 3 true) in
  assert_bool "Placement échoue pour un bateau se superposant" superpose

(* Test de flotte complète *)
let test_flotte_complete _ =
  let plateau = init_plateau () in
  List.iteri
    (fun i taille -> ignore (placer_bateau_valide plateau i i taille true))
    flotte_standard;
  assert_bool "Flotte complète" (flotte_complete plateau);
  (* On ajoute un bateau supplémentaire qui ne fait pas partie de flotte_standard *)
  ignore (placer_bateau_valide plateau 9 0 1 true);
  assert_bool "Flotte avec un bateau en trop" (not (flotte_complete plateau))

(* Test aléatoire de placement de bateaux avec QCheck *)
let test_qcheck_placements_aleatoires =
  QCheck.Test.make ~name:"Test Placement Aléatoire" ~count:1000
    (QCheck.pair (QCheck.int_bound 9) (QCheck.int_bound 9))
    (fun (x, y) ->
      let plateau = init_plateau () in
      (* Essaie de placer un bateau de taille 3 à la position aléatoire (x, y) *)
      let place = placer_bateau_valide plateau x y 3 true in
      (* Vérifie que si l'emplacement est valide, alors le placement a bien réussi *)
      if emplacement_valide plateau (generer_bateau x y 3 true) then place
      else true
      (* Si l'emplacement n'est pas valide, considère que le test passe *))

(* Test pour obtenir_plateau_cache *)
let test_obtenir_plateau_cache _ =
  (* Création d'un plateau et placement de bateaux *)
  let p = init_plateau () in
  ignore (placer_bateau_valide p 0 0 3 true);
  (* Bateau de taille 3 en haut à gauche *)
  ignore (placer_bateau_valide p 5 5 2 false);
  (* Marquage de quelques cases comme Touche et Rate *)
  tir p (0, 1);
  tir p (7, 7);
  (* Obtenir la version cachée du plateau *)
  let plateau_cache = obtenir_plateau_cache p in

  (* Vérification que les cases de bateau sont bien marquées comme Vide *)
  assert_equal plateau_cache.(0).(0) Vide;
  assert_equal plateau_cache.(0).(2) Vide;
  assert_equal plateau_cache.(5).(5) Vide;
  assert_equal plateau_cache.(6).(5) Vide;

  (* Vérification que les cases Touche et Rate restent inchangées *)
  assert_equal plateau_cache.(0).(1) Touche;
  assert_equal plateau_cache.(7).(7) Rate;

  (* Vérification que les cases vides restent vides *)
  assert_equal plateau_cache.(9).(9) Vide

(* Test pour la fonction tir *)
let test_tir _ =
  let p = init_plateau () in
  ignore (placer_bateau_valide p 0 0 3 true);
  (* Place un bateau de taille 3 horizontalement en (0, 0) *)
  tir p (0, 0);
  (* Vérifie que la case (0, 0) est marquée comme Touche *)
  tir p (1, 1);
  (* Vérifie que la case (1, 1) est marquée comme Rate, car elle est vide *)
  tir p (0, 1);
  (* Vérifie qu'un tir sur une autre partie du bateau (0, 1) est également marqué comme Touche *)
  let res = obtenir_plateau_cache p in
  assert_equal res.(0).(0) Touche;
  assert_equal res.(1).(1) Rate;
  assert_equal res.(0).(1) Touche

(* Regroupement de tous les tests *)
let suite =
  "Test Plateau Bataille Navale"
  >::: [
         "Initialisation du plateau" >:: test_init_plateau;
         "Emplacement valide" >:: test_emplacement_valide;
         "Placement de bateau valide" >:: test_placer_bateau_valide;
         "Flotte complète" >:: test_flotte_complete;
         "Obtenir plateau caché" >:: test_obtenir_plateau_cache;
         "Tir" >:: test_tir;
       ]

(* Lancement des tests *)
let run =
  run_test_tt_main suite;
  QCheck.Test.check_exn test_qcheck_placements_aleatoires
