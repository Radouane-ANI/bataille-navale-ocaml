module Game = struct
  
open Plateau

type game_state = {
  board_p1 : Plateau.plateau;
  board_p2 : Plateau.plateau;
  current_player : player;
}

and player = Player1 | Player2

type game_view = Plateau.case array array (*attention a ne pas donner les valeur de plateau dans gameview*)

type play = int * int  

type error = 
  | Invalid_move 
  | Position_out_of_bounds

type outcome =
  | Next of game_state
  | Error of error
  | Endgame of player option

let view (game_state : game_state) (player : player) : Plateau.plateau =
  (* Retourne le plateau de l'adversaire caché pour le joueur en cours *)
  match player with
  | Player1 -> game_state.board_p2
  | Player2 -> game_state.board_p1

let act (_player : player) (_play : play) (_game_state : game_state) : outcome =
  Endgame None
  
end
