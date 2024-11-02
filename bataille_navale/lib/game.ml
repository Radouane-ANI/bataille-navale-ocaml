type game_state = {
  board_p1 : Plateau.plateau;
  board_p2 : Plateau.plateau;
  current_player : player;
}

and player = Player1 | Player2

type game_view = Plateau.case array array
type play = int * int
type error = Invalid_move | Position_out_of_bounds
type outcome = Next of game_state | Error of error | Endgame of player option

let init_game (board_p1 : Plateau.plateau) (board_p2 : Plateau.plateau) =
  { board_p1; board_p2; current_player = Player1 }

let view (game_state : game_state) (player : player) : game_view =
  (* Retourne le plateau de l'adversaire caché pour le joueur en cours *)
  match player with
  | Player1 -> Plateau.obtenir_plateau_cache game_state.board_p2
  | Player2 -> Plateau.obtenir_plateau_cache game_state.board_p1

let act (_player : player) (_play : play) (_game_state : game_state) : outcome =
  Endgame None

let display (game_view : game_view) : unit = Plateau.afficher_grille game_view
