#use "code.ml";;

(** Algorithmes de recherche de code *)
module IA :
sig
(** Nombre d'algorithmes developpes *)
val nombre_methodes : int
(** Choisit un code a proposer
* @param methode 0 pour l'algorithme naif,
* 1 pour l'algorithme de KNUTH
* ... et ainsi de suite
* @param essais la liste des codes deja proposes
* @param possibles la liste des codes possibles
* @return le prochain code a essayer
*)
val choix : int -> Code.t list -> Code.t list -> Code.t
(** Filtre les codes possibles
* @param methode 0 pour l'algorithme naif,
* 1 pour l'algorithme de KNUTH
* ... et ainsi de suite
* @param (code, rep) le code essaye et la reponse correspondante
* @param possibles la liste de courante de codes possibles
* @return la nouvelle liste de codes possibles
*)
val filtre : int -> (Code.t * (int * int) option) -> Code.t list -> Code.t list
end =
  struct

    let nombre_methodes = 2;;
    
    let rec code_egaux code1 code2 =
      match (code1,code2) with
      | ([],[]) -> true
      | (a :: ls1,b :: ls2) -> if a = b then
                                 code_egaux ls1 ls2  
                               else
                                 false
      | _ -> false;;
    
    let rec existe code essais =
      match essais with
      | [] -> false
      | v :: ls -> if code_egaux code v then
                     true
                   else
                     existe code ls;;
    
    let rec filtre_choix_knuth code rep possibles acc =
      match possibles with
      | [] -> acc
      | v :: ls -> match ((Code.reponse code v),rep) with
                   | (None,_) -> filtre_choix_knuth code rep ls acc
                   | (Some(a,b),(x,y)) -> if (a = x) && (b = y) then
                                            filtre_choix_knuth code rep ls (acc+1)
                                          else
                                            filtre_choix_knuth code rep ls acc;;
    
    let poids code possibles = let deb = filtre_choix_knuth code (List.hd (Code.toutes_reponses)) possibles 0 and fin = List.tl Code.toutes_reponses in
                               List.fold_left (fun acc x -> let y = filtre_choix_knuth code x possibles 0 in if acc > y then acc else y) deb fin;;
    
    let rec choix_knuth_rec essais possibles liste_tous res =
      match liste_tous with
      | [] -> snd(res)
      | v :: ls when existe v essais -> choix_knuth_rec essais possibles ls res
      | v :: ls -> let x = poids v possibles in if x < fst(res) then
                                                  choix_knuth_rec essais possibles ls (x,v)
                                                else
                                                  choix_knuth_rec essais possibles ls res;;
    
    let rec choix_knuth essais possibles liste_tous =
      match liste_tous with
      | [] -> failwith "choix_knuth"
      | v :: ls when existe v essais -> choix_knuth essais possibles ls
      | v :: ls -> choix_knuth_rec essais possibles ls ((poids v possibles),v);; 
    
    let choix methode essais possibles =
      match methode with
      | 1 -> List.hd possibles
      | _ -> choix_knuth essais possibles Code.tous ;;
    
    let rec supprime_un x l =
      match l with
      | [] -> l
      | valeur :: ls -> if code_egaux x valeur then
                          ls
                        else
                      valeur :: supprime_un x ls;;
    
    let rec filtre_naif code rep possibles =
      match possibles with
      | [] -> possibles
      | v :: ls -> let suivant = filtre_naif code rep ls in
                   match ((Code.reponse code v),rep) with
                   | (None,_) -> suivant
                   | (Some(a,b),Some(x,y)) -> if (a = x) && (b = y) then
                                                v :: suivant
                                              else
                                                suivant;;
    
    let filtre methode der_code possibles =
      match methode with
      | 1 -> filtre_naif (fst(der_code)) (snd(der_code)) (supprime_un (fst(der_code)) possibles)
      | _ -> filtre_naif (fst(der_code)) (snd(der_code)) (supprime_un (fst(der_code)) possibles);;
    
  end ;;
