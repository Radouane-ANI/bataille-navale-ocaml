type game_state = {
  board_p1 : Plateau.plateau;
  board_p2 : Plateau.plateau;
  mutable current_player : player;
}

and player = Player1 | Player2

type game_view = Plateau.case array array
type play = int * int
type error = Position_out_of_bounds | Not_Player_Turn
type outcome = Next of game_state | Error of error | Endgame of player option

let init_game (board_p1 : Plateau.plateau) (board_p2 : Plateau.plateau) =
  { board_p1; board_p2; current_player = Player1 }

let view (game_state : game_state) (player : player) : game_view =
  (* Retourne le plateau de l'adversaire caché pour le joueur en cours *)
  match player with
  | Player1 -> Plateau.obtenir_plateau_cache game_state.board_p2
  | Player2 -> Plateau.obtenir_plateau_cache game_state.board_p1

let act (player : player) (play : play) (game_state : game_state) : outcome =
  if player <> game_state.current_player then Error Not_Player_Turn
  else if (fst play < 0 || fst play > 9) || snd play < 0 || snd play > 9 then
    Error Position_out_of_bounds
  else
    let prochainne_action plateau proch_joueur =
      if Plateau.endgame plateau then Endgame (Some proch_joueur)
      else (
        game_state.current_player <- proch_joueur;
        Next game_state)
    in
    match player with
    | Player1 ->
        Plateau.tir game_state.board_p2 play;
        prochainne_action game_state.board_p2 Player2
    | Player2 ->
        Plateau.tir game_state.board_p1 play;
        prochainne_action game_state.board_p1 Player1

let display (game_view : game_view) : unit = Plateau.afficher_grille game_view
