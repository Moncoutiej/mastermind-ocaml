module Affichage = struct
	

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


let rec afficher_code_rec code =
	match code with
	| [] -> print_string "\027[37m"
	| 0 :: liste -> print_string ("\027[31m ● ");afficher_code_rec liste
	| 1 :: liste -> print_string ("\027[31m ● ");afficher_code_rec liste
	| 2 :: liste -> print_string ("\027[32m ● ");afficher_code_rec liste
	| 3 :: liste -> print_string ("\027[34m ● ");afficher_code_rec liste
	| 4 :: liste -> print_string ("\027[33m ● ");afficher_code_rec liste
	| 5 :: liste -> print_string ("\027[35m ● ");afficher_code_rec liste
	| 6 :: liste -> print_string ("\027[37m ● ");afficher_code_rec liste
	| 7 :: liste -> print_string ("\027[36m ● ");afficher_code_rec liste;;

let afficher_code code = print_string "|"; afficher_code_rec code; print_string "|";;

let afficher_reponse rep =
	match rep with
	| (a,b) -> print_string ("     BP : "^string_of_int a ^"     MP : "^string_of_int b);;

let rec affiche_plusieurs_codes_et_reponses liste_codes liste_reponses = 
	match (liste_codes,liste_reponses) with
	| ([],[]) -> ()
	| (v1 :: liste1,v2 :: liste2) -> afficher_code v1;afficher_reponse v2;print_newline ();affiche_plusieurs_codes_et_reponses liste1 liste2;;







end;;
open Affichage;;

affiche_plusieurs_codes_et_reponses [[1;2;3;4];[1;4;5;6];[3;4;1;6]] [(2,1);(3,0);(0,4)];;

(*
Graphics.open_graph " 1048x725";;
Affichage.int_of_couleur 2;;
point_color 200 200;;
close_graph;;*)