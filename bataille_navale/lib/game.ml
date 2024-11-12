type game_state = {
  board_p1 : Plateau.plateau;
  board_p2 : Plateau.plateau;
  mutable current_player : player;
}

and player = Player1 | Player2

type game_view = Plateau.case array array
type play = int * int
type error = Position_out_of_bounds | Not_Player_Turn | Invalid_board
type outcome = Next of game_state | Error of error | Endgame of player option

(** Initialise une nouvelle partie de Bataille Navale avec les plateaux des deux joueurs.
    @param board_p1 Plateau du joueur 1
    @param board_p2 Plateau du joueur 2
    @return L'état initial du jeu ou une erreur si les flottes ne sont pas complètes *)
let init_game (board_p1 : Plateau.plateau) (board_p2 : Plateau.plateau) : outcome =
  if Plateau.flotte_complete board_p1 && Plateau.flotte_complete board_p2 then (* erreur de verif *)
    Next { board_p1; board_p2; current_player = Player1 }
  else Error Invalid_board

let view (game_state : game_state) (player : player) : game_view =
  (* Retourne le plateau de l'adversaire caché pour le joueur en cours *)
  match player with
  | Player1 -> Plateau.obtenir_plateau_cache game_state.board_p2
  | Player2 -> Plateau.obtenir_plateau_cache game_state.board_p1


(** Vérifie si une position est valide sur la grille de jeu (entre 0 et 9 inclus).
    @param x Coordonnée x
    @param y Coordonnée y
    @return true si la position est valide, false sinon *)
let is_position_valid (x : int) (y : int) : bool =
  x >= 0 && x <= 9 && y >= 0 && y <= 9

(** Exécute un tour de jeu en effectuant un tir sur la position donnée.
    @param player Le joueur effectuant le tir
    @param play La position ciblée par le tir
    @param game_state L'état actuel du jeu
    @return L'état du jeu après le tir ou une erreur si le tir est invalide *)
let act (player : player) (play : play) (game_state : game_state) : outcome =
  if player <> game_state.current_player then
    Error Not_Player_Turn
  else
    let x, y = play in
    if not (is_position_valid x y) then
      Error Position_out_of_bounds
    else
      let target_board, next_player =
        match player with
        | Player1 -> (game_state.board_p2, Player2)
        | Player2 -> (game_state.board_p1, Player1)
      in
      Plateau.tir target_board play;
      if Plateau.endgame target_board then
        Endgame (Some next_player)
      else (
        game_state.current_player <- next_player;
        Next game_state
      )

let display (game_view : game_view) : unit = Plateau.afficher_grille game_view
