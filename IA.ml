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


    
    let choix methode essais possibles =
      match methode with
      | _ -> List.hd possibles;;

    let rec code_egaux code1 code2 acc =
      match (code1,code2) with
      | ([],[]) -> acc = 4
      | (a :: ls1,b :: ls2) -> if a = b then
                                 code_egaux ls1 ls2 (acc+1)  
                               else
                                 code_egaux ls1 ls2 acc ;;
  
    let rec supprime_un x l =
      match l with
      | [] -> l
      | valeur :: ls -> if code_egaux x valeur 0 then
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
      | _ -> naif fst(der_code) snd(der_code) (supprime_un (fst(der_code)) possibles);;

  
  end ;;
