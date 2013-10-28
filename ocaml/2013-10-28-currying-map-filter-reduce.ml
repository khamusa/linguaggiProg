(* Currying è il nome che si da per quello che abbiamo chiamato spesso partial function evaluation *)
(* Consiste nel legare soltanto parte degli argomenti di una funzione *)

let compose f x = f (g x);;
let compose' = f (x -> x + 1);;

let compose ~f  x = f (g x);;
(*  È possibile legare anche il secondo argomento invece del primo, per quello basta utilizzare i named parameter  *)
let compose' = compose ~g: (fun x -> x + 1);;


(* MAP, FILTER E REDUCE *)

(* Map applica una funzione a tutti gli elementi della lista e restituisce una nuova lista *)
(* Filter rimuove gli ementi per cui la valutazione della funzione restituisce false *)
(* Reduce permette di portare avanti un accumulatore (come la fold i F#) *)

(* La semantica della map è la seguente *)
let rec map f = function
	h :: l1 -> f h::map f l1 (* viene eseguito come f h::(map f l1) *)
	| _ -> [];;

(* Semantica della filter: *)
let rec filter f = function
	[] -> []
	| h::l -> if p h then h :: filter p l else filter p
	(* Ovvero, costruisce una nuova lista costituita dagli elementi della lista originale per cui
		la funzione f restituisce true *)

