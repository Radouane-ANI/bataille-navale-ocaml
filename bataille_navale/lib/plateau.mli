type bateau
type case = Vide | Bateau of bateau | Touche | Rate
type plateau

val plateau_vide : case array array
val flotte_standard : int list
val init_plateau : unit -> plateau
val emplacement_valide : plateau -> bateau -> bool
val generer_bateau : int -> int -> int -> bool -> bateau
val placer_bateau_valide : plateau -> int -> int -> int -> bool -> bool
val flotte_complete : plateau -> bool
