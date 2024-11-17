type game_state
type player = Player1 | Player2
type play = int * int
type error = Position_out_of_bounds | Not_Player_Turn | Invalid_board
type outcome = Next of game_state | Error of error | Endgame of player option

val init_game : Plateau.plateau -> Plateau.plateau -> outcome
val view : game_state -> player -> Game_view.game_view
val act : player -> play -> game_state -> outcome
