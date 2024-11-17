type game_view = Plateau.case array array

let display (game_view : game_view) : unit = Plateau.afficher_grille game_view
