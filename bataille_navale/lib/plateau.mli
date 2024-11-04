type bateau
type case = Vide | Bateau of bateau | Touche | Rate | Coule
type plateau

val plateau_vide : case array array
val flotte_standard : int list
val init_plateau : unit -> plateau
val emplacement_valide : plateau -> bateau -> bool
val generer_bateau : int -> int -> int -> bool -> bateau
val placer_bateau_valide : plateau -> int -> int -> int -> bool -> bool
val flotte_complete : plateau -> bool
val obtenir_plateau_cache : plateau -> case array array
val supp_ships_list : (int * int) list list -> int * int -> (int * int) list list
val bateau_touche : plateau -> int * int -> unit
val tir : plateau -> int * int -> unit
val endgame : plateau -> bool
val afficher_plateau : plateau -> unit
val afficher_plateau_in_game : plateau -> unit 
val afficher_grille : case array array -> unit
