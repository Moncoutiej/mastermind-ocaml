#use "affichage.ml";;
#use "IA.ml";;
module Mastermind = struct
	
type joueur = {nom : string; points : int; humain : bool};;
type joueurs = {un : joueur; deux : joueur};;

let creationjoueurs mode nom_joueur = if mode then 
                             {un = {nom = nom_joueur; points = 0; humain = true}; deux = {nom = "Ordinateur"; points = 0; humain =false}}
                           else 
                             {un = {nom = read_line (print_string "Entrer votre nom joueur 2 : "); points = 0; humain = true}; deux = {nom = read_line (print_string "Entrer votre nom joueur 1 : "); points = 0; humain =true}};;

let commence couplejoueurs = let b = Random.bool() in
                             match couplejoueurs with
			                       | {un = _; deux = _} when b -> couplejoueurs
			                       | {un = x; deux = y} -> {un = y; deux = x};; 

let nb_parties_pairs n = if (n mod 2 = 0) then n else (n+1);;

let methode = read_int ((print_string "Choisir la méthode, 1 : Naïf | 2 : Knuth : "));; 

let rec random_code nb_pion couleur_max acc =
  if nb_pion = 0 then
    acc
  else
    random_code (nb_pion-1) couleur_max (Random.int couleur_max :: acc);;

let creation_random_code = fun () -> random_code Code.nombre_pions (List.length Code.couleurs_possibles) [];; 


let rec saisie_ordi_humain participant = if participant = {nom = "Ordinateur"; points = participant.points; humain =false} 
                                           then creation_random_code ()
                                           else let saisie = read_line ((print_string "Creez le code caché : ")) 
                                                in match Code.code_of_string saisie with
                                                    | None -> print_string "Saisie Invalide"; saisie_ordi_humain participant
                                                    | Some(x) -> x;;

let rec saisie_humain participant = let saisie = read_line ((print_string "Tentez un code : ")) 
                                                in match Code.code_of_string saisie with
                                                    | None -> print_string "Saisie Invalide"; saisie_humain participant
                                                    | Some(x) -> x;;

let rec run_tentatives_IA tentatives vrai_code participant possibles essaye  =
  match tentatives with
  | 0 -> participant 
  | e -> let code = (IA.choix methode essaye possibles) in let reponse = Code.reponse code vrai_code in match reponse with
                                                       | None -> failwith "run_tentatives_IA"
                                                       | Some(a,b) when a = List.length vrai_code -> Affichage.afficher_code code; Affichage.afficher_reponse (a,b); print_newline (); {nom = participant.nom ; points = participant.points+1 ; humain = participant.humain}
                                                       | Some(a,b) -> let nw_possibles = IA.filtre methode (code,reponse) possibles in Affichage.afficher_code code; Affichage.afficher_reponse (a,b); print_newline (); run_tentatives_IA (e-1) vrai_code participant nw_possibles (code :: essaye) ;;

let rec run_tentatives_humain tentatives vrai_code participant essaye = 
  match tentatives with
  | 0 -> participant 
  | e -> let code = (saisie_humain participant) in let reponse = Code.reponse code vrai_code in match reponse with
                                                       | None -> failwith "run_tentatives_humain"
                                                       | Some(a,b) when a = List.length vrai_code -> Affichage.afficher_code code; Affichage.afficher_reponse (a,b); print_newline (); {nom = participant.nom ; points = participant.points+1 ; humain = participant.humain}
                                                       | Some(a,b) -> Affichage.afficher_code code; Affichage.afficher_reponse (a,b); print_newline (); run_tentatives_humain (e-1) vrai_code participant (code::essaye);;

let rec run_parties parties couplejoueurs tentatives  =
  match parties with
  | 0 -> couplejoueurs
  | e when (e mod 2 = 0) && couplejoueurs.un.humain -> print_string("1");  let participant = run_tentatives_IA tentatives (saisie_ordi_humain couplejoueurs.un) couplejoueurs.deux Code.tous [[]] 
                               in run_parties (e-1) {un = couplejoueurs.un; deux = participant} tentatives  
  | e when (e mod 2 <> 0) && couplejoueurs.deux.humain -> print_string("2"); let participant = run_tentatives_IA tentatives (saisie_ordi_humain couplejoueurs.deux) couplejoueurs.un Code.tous [[]] 
                               in run_parties (e-1) {un = participant; deux = couplejoueurs.deux} tentatives 
  | e when (e mod 2 = 0) && (not couplejoueurs.un.humain) -> print_string("3"); let participant = run_tentatives_humain tentatives (saisie_ordi_humain couplejoueurs.un) couplejoueurs.deux [[]] 
                               in run_parties (e-1) {un = couplejoueurs.un; deux = participant} tentatives 
  | e when (e mod 2 <> 0) && (not couplejoueurs.deux.humain) -> print_string("4");let participant = run_tentatives_humain tentatives (saisie_ordi_humain couplejoueurs.deux) couplejoueurs.un [[]]
                               in run_parties (e-1) {un = participant; deux = couplejoueurs.deux} tentatives;;

let rec reponse_humain code vrai_code = let (a,b) = (read_int ((print_string "Entrez le nombre de bien placé : ")), read_int ((print_string "Entrez le nombre de mal placé : "))) in 
                                        match Code.reponse code vrai_code with
                                        | None -> failwith "reponse_humain"
                                        | Some(x,y) when (a<>x) || (b<> y) -> failwith "Vous vous etes trompe ou vous avez essaye de tricher !!!"
                                        | Some(x,y) when (a = x) && (b = y) -> Some(x,y);;



let rec run_reponse_pas_auto tentatives vrai_code participant possibles essaye  =
  match tentatives with
  | 0 -> participant 
  | e -> let code = (IA.choix methode essaye possibles) in Affichage.afficher_code code; print_newline (); let reponse = reponse_humain code vrai_code in match reponse with
                                                       | None -> failwith "run_tentatives_IA"
                                                       | Some(a,b) when a = List.length vrai_code -> Affichage.afficher_code code; Affichage.afficher_reponse (a,b); print_newline (); {nom = participant.nom ; points = participant.points+1 ; humain = participant.humain}
                                                       | Some(a,b) -> let nw_possibles = IA.filtre methode (code,reponse) possibles in Affichage.afficher_code code; Affichage.afficher_reponse (a,b); print_newline (); run_reponse_pas_auto (e-1) vrai_code participant nw_possibles (code :: essaye) ;;


let rec run_parties_pas_auto parties couplejoueurs tentatives =
  match parties with
  | 0 -> couplejoueurs
  | e when (e mod 2 = 0) && couplejoueurs.un.humain -> let participant = run_reponse_pas_auto tentatives (saisie_ordi_humain couplejoueurs.un) couplejoueurs.deux Code.tous [[]] 
                               in run_parties_pas_auto (e-1) {un = couplejoueurs.un; deux = participant} tentatives 
  | e when (e mod 2 <> 0) && couplejoueurs.deux.humain -> let participant = run_reponse_pas_auto tentatives (saisie_ordi_humain couplejoueurs.deux) couplejoueurs.un Code.tous [[]] 
                               in run_parties_pas_auto (e-1) {un = participant; deux = couplejoueurs.deux} tentatives 
  | e when (e mod 2 = 0) && (not couplejoueurs.un.humain) -> let participant = run_tentatives_humain tentatives (saisie_ordi_humain couplejoueurs.un) couplejoueurs.deux [[]]
                               in run_parties_pas_auto (e-1) {un = couplejoueurs.un; deux = participant} tentatives 
  | e when (e mod 2 <> 0) && (not couplejoueurs.deux.humain) -> let participant = run_tentatives_humain tentatives (saisie_ordi_humain couplejoueurs.deux) couplejoueurs.un [[]]
                               in run_parties_pas_auto (e-1) {un = participant; deux = couplejoueurs.deux} tentatives;;

let gagnant couplejoueurs = 
  match couplejoueurs.un.points with
  | points when (points = couplejoueurs.deux.points) -> print_string ("Il y'a égalité")
  | points when (points < couplejoueurs.deux.points) -> print_string ("Le gagnant est le joueur 2") 
  | points when (points > couplejoueurs.deux.points) -> print_string ("Le gagnant est le joueur 1") ;;




let rec mastermind nom tentatives parties auto =
  let participants = creationjoueurs true nom in 
    let couplejoueurs = commence participants in let partie_pairs = nb_parties_pairs parties in 
                                                                    match auto with
                                                                    | true -> let winner = run_parties partie_pairs couplejoueurs tentatives  in gagnant winner 
                                                                    | false -> let winner = run_parties_pas_auto partie_pairs couplejoueurs tentatives in gagnant winner;; 
                                                                    
    
                                                 
                                                
end;;
open Mastermind;;
Mastermind.mastermind "Luca" 6 1 true;;
(*Mastermind.run_parties 2 {un = {nom = "Ordinateur"; points = 0; humain = false}; deux = {nom = "Luca"; points = 0; humain =true}} 10;;*)
(*Mastermind.run_parties 3 {un = {nom = "Luca"; points = 0; humain = true}; deux = {nom = "Ordinateur"; points = 0; humain =false}} 6;;*)

