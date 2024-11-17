  (* Variable mutable pour contrôler le mode verbose *)
  let verbose = ref false

  (* Fonction pour activer ou désactiver les logs *)
  let set_verbose v = verbose := v

  (* Fonction pour afficher un message si verbose est activé *)
  let log message =
    if !verbose then print_endline message

