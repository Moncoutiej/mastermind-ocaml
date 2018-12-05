(** Module de definition d'un code dans le jeu Mastermind *)
module Code :
sig
(** Le type d'un pion *)
type pion = int
(** Le type d'un code *)
type t = pion list
(** Nombre de pions par code *)
val nombre_pions : int
(** Liste des couleurs possibles *)
val couleurs_possibles : pion list
(** Compare deux codes
* @param code1 premier code a comparer
* @param code2 second code a comparer
* @return 0 si les deux codes sont identiques,
un entier positif si [code1] est strictement plus grand que [code2]
un entier negatif si [code1] est strictement plus petit que [code2]
*)
val compare : t -> t -> int
(** Conversion code vers chaine de caracteres (pour affichage)
* @param code code a convertir
* @return la representation en chaine de caracteres de [code]
*)
val string_of_code : t -> string
(** Conversion chaine de caracteres vers code (pour saisie)
* @param string chaine de caractere saisie
* @return le code correspondant a la saisie si la conversion est possible
[None] si la conversion n'est pas possible
*)
val code_of_string : string -> t option
(** La liste de tous les codes permis *)
val tous : t list;;
(** La liste de toutes les reponses possibles *)
val toutes_reponses : (int * int) list 
(** Calcule la reponse d'un code par rapport au code cache
* @param code le code propose
* @param vrai_code le code cache
* @return un couple (nombre de pions bien places, nombre de pions mal places)
[None] si la reponse ne peut etre calculee
*)
val reponse : t -> t -> (int * int) option
end =
struct 

let rec string_of_code_rec t = 
	"|"^
	match t with
	| [] -> ""
	| 0 :: liste -> "orange"^string_of_code_rec liste
	| 1 :: liste -> "rouge"^string_of_code_rec liste
	| 2 :: liste -> "vert"^string_of_code_rec liste
	| 3 :: liste -> "bleu"^string_of_code_rec liste
	| 4 :: liste -> "jaune"^string_of_code_rec liste
	| 5 :: liste -> "violet"^string_of_code_rec liste
	| 6 :: liste -> "blanc"^string_of_code_rec liste
	| 7 :: liste -> "cyan"^string_of_code_rec liste;;

let string_of_code t = string_of_code_rec t ^ "|";;

let num_of_couleur_opt s = 
	match s with
	| "orange" -> Some(0) 
	| "rouge" -> Some(1) 
	| "vert" -> Some(2) 
	| "bleu" -> Some(3) 
	| "jaune" -> Some(4) 
	| "violet" -> Some(5) 
	| "blanc" -> Some(6) 
	| "cyan" -> Some(7) 
	| _ -> None;;

let rec code_of_string s =
	if s = "|" then
	 Some([])
	else
	 let i_deuxieme_separateur = String.index_from s 1 '|' in
	  let s_prem_couleur = String.sub s 1 (i_deuxieme_separateur -1) in
	   let suite_s = String.sub s i_deuxieme_separateur (String.rindex s '|' -i_deuxieme_separateur +1) in
	    let pion = num_of_couleur_opt s_prem_couleur in
	     let apl_rec = code_of_string suite_s in
			match (pion,apl_rec) with
			| (Some(a),Some(b)) -> Some(a :: b)
			| _ -> None;;

end ;;


"|rouge|bleu|gfgff|gg|"