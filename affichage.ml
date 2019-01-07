#use "code.ml"
module Affichage : sig

(** Afffiche un code avec des séparateurs au début et à la fin
* @param code à écrire dans le terminal
*)
val afficher_code : Code.t -> unit

(** Afffiche une réponse dans le terminal
* @param réponse à écrire dans le terminal
*)
val afficher_reponse : (int * int) -> unit

(** Afffiche un code et sa réponse associée dans le terminal à l'aide des fonctions précédentes
* @param la liste de codes à écrire dans le terminal
* @param la liste de réponses associées aux codes dans la liste précedente
*)
val affiche_plusieurs_codes_et_reponses : Code.t list -> (int * int) list -> unit

(** Afffiche les couleurs possibles pour l'utilisateur
* @param la taille de la liste de couleurs
*)
val afficher_couleurs_possibles : int -> unit 

end


 = struct
	

(*
let int_of_couleur valeur = 
	match valeur with
	| 1 -> Graphics.set_color Graphics.red 
	| 2 -> Graphics.set_color Graphics.green
	| 3 -> Graphics.set_color Graphics.blue
	| 4 -> Graphics.set_color Graphics.yellow
	| 5 -> Graphics.set_color Graphics.magenta
	| 6 -> Graphics.set_color Graphics.white
	| _ -> Graphics.set_color Graphics.cyan;;
*)

(** Afffiche un code dans le terminal tel quel avec des points colorés
* @param code à écrire dans le terminal
*)
let rec afficher_code_rec code =
	match code with
	| [] -> print_string "\027[37m"
	| 0 :: liste -> print_string ("\027[31m ● ");afficher_code_rec liste
	| 1 :: liste -> print_string ("\027[32m ● ");afficher_code_rec liste
	| 2 :: liste -> print_string ("\027[34m ● ");afficher_code_rec liste
	| 3 :: liste -> print_string ("\027[33m ● ");afficher_code_rec liste
	| 4 :: liste -> print_string ("\027[35m ● ");afficher_code_rec liste
	| 5 :: liste -> print_string ("\027[37m ● ");afficher_code_rec liste
	| 6 :: liste -> print_string ("\027[36m ● ");afficher_code_rec liste;;


let afficher_code code = print_string "\027[37m |"; afficher_code_rec code; print_string "|";;

let afficher_reponse rep =
	match rep with
	| (a,b) -> print_string ("     BP : "^string_of_int a ^"     MP : "^string_of_int b);;


let afficher_couleurs_possibles taille_liste_couleurs = 
    match taille_liste_couleurs with
	| 0 -> print_string "aucunes couleurs"
	| 1 -> print_string "La seule couleur possible est : rouge"
	| 2 -> print_string "Les couleurs possibles sont : rouge, vert"
	| 3 -> print_string "Les couleurs possibles sont : rouge, vert, bleu"
	| 4 -> print_string "Les couleurs possibles sont : rouge, vert, bleu, jaune"
	| 5 -> print_string "Les couleurs possibles sont : rouge, vert, bleu, jaune, violet"
	| 6 -> print_string "Les couleurs possibles sont : rouge, vert, bleu, jaune, violet, blanc"
	| 7 -> print_string "Les couleurs possibles sont : rouge, vert, bleu, jaune, violet, blanc, cyan"
	| _ -> failwith "erreur taille liste couleur";;



let rec affiche_plusieurs_codes_et_reponses liste_codes liste_reponses = 
	match (liste_codes,liste_reponses) with
	| ([[]],[]) -> print_string ""
	| (v1 :: liste1,v2 :: liste2) -> let res = affiche_plusieurs_codes_et_reponses liste1 liste2 in afficher_code v1;afficher_reponse v2;print_newline ();;





end;;
open Affichage;;

(*
Graphics.open_graph " 1048x725";;
Affichage.int_of_couleur 2;;
point_color 200 200;;
close_graph;;*)
