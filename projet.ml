#use "code.ml";;
#use "IA.ml";;
module Mastermind = struct
	
type joueur = {nom : string; points : int; humain : bool};;
type joueurs = joueur list;;

let creationjoueurs mode = if mode then 
{nom = read_line (print_string "Entrer votre nom : "); points = 0; humain = true} :: [{nom = "Ordinateur"; points = 0; humain =false}] else 
{nom = read_line (print_string "Entrer votre nom joueur 2 : "); points = 0; humain = true} :: [{nom = read_line (print_string "Entrer votre nom joueur 1 : "); points = 0; humain =true}];;

let commence listejoueurs = if Random.bool() then match listejoueurs with
                                                  | e :: t -> [e]
                            else match listejoueurs with
                                  | e :: t -> t;;



let rec code_egaux code1 code2 acc =
	match (code1,code2) with
	| ([],[]) -> acc = 4
	| (a :: ls1, b :: ls2) -> if a = b then code_egaux ls1 ls2 (acc+1) 
							  else code_egaux ls1 ls2 acc;;

(*let rec ajout_points acc = if code_egaux code1*)




end;;
open Mastermind;;

