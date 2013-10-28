(* Currying è il nome che si da per quello che abbiamo chiamato spesso partial function evaluation *)
(* Consiste nel legare soltanto parte degli argomenti di una funzione *)

let compose f g x = f (g x);;
let compose' = compose (x -> x + 1);;
(* compose' sarà una funzione definita cosi: let compose' = (g x) + 1;;
	ovvero, abbiamo sostituito la f nella funzione originale per x -> x + 1 *)

let compose ~f g x = f (g x);;
(*  È possibile legare anche il secondo argomento invece del primo, per quello basta utilizzare i named parameter  *)
let compose' = compose ~g: (fun x -> x + 1);;


(* MAP, FILTER E REDUCE *)

(* Map applica una funzione a tutti gli elementi della lista e restituisce una nuova lista *)
(* Filter rimuove gli ementi per cui la valutazione della funzione restituisce false *)
(* Reduce permette di portare avanti un accumulatore (come la fold i F#) *)

(* La semantica della map è la seguente (non ottimizzata! Non è ricorsione di coda) *)
let rec map f = function
	h :: l1 -> f h::map f l1 (* viene eseguito come f h::(map f l1) *)
	| _ -> [];;

(* Semantica della filter: (non ottimizzata! Non è ricorsione di coda) *)
let rec filter f = function
	[] -> []
	| h::l -> if p h then h :: filter p l else filter p
	(* Ovvero, costruisce una nuova lista costituita dagli elementi della lista originale per cui
		la funzione f restituisce true *)

let rec reduce acc op = function
	[] -> acc| h::tl -> reduce (op acc h) op tl ;;

(* Map e reduce possono essere utilizzate insieme per controllare se esiste un valore
	in una lista per cui la valutazione di un predicato ritorna true *)
let exists p l = reduce false (or) (map p l);;

let rec myexists p = function
	[] -> false
	| h::tl -> if p h then true else myexists p tl


(* forall returns true se tutti gli elementi E appartenenti alla lista soddisfano
	p E = true *)
let forall p l = reduce true (&) (map p l);;


(* La reduce in realtà è un caso particolare di quello che viene chiamato folding *)
(* La reduce che abbiamo visto coincide con la fold_left *)
(* La fold_right ha lo stesso comportamento, ma incominicia da destra, ovver
	dall'ultimo elemento della lista *)

(* Zip -> ci permette di accoppiare gli elementi di due liste in una nuova lista di coppie,
	usando la lista più piccola come punto di riferimento *)
let zip_longest l1 l2 =
		match (l1, l2) with
		([], []) | ([], _) | (_, []) -> []
		| (h1::l1', h2::l2') -> (h1, h2) :: (zip_longest l1' l2');;

(* Group by *)
(*  Group by divide gli elementi di una lista in sottoliste indicizzate 
	dal valore resituito dall'applicazione di f su ogni elemento *)
type 'a group = { mutable g: 'a list }
let empty_group = function x -> { g = [] };;

let rec group_by l ?(ris:'a group array = (Array.init 10 empty_group)) f =
	match l with
	[] -> ris
	| h::l1 -> 
		(ris.((f h)).g <- ris.((f h)).g @ [h] ;
		group_by l1 ~ris:ris f );;

(* Nota: Gli array non sono matchabili *)

(* crea una lista di coppie costituite da ogni elemento della lista accoppiato con il suo
	successivo *)
let rec pairwise = function
	h'::h''::l' -> (h', h'')::pairwise(h''::l')
	| _ -> []

(* Funzioni con numero variabile di argomenti *)
(*  In C e Java si usa il meccanismo di nome varargs, in python si chiama Ellipsis *)
(* Non abbiamo in ML, ma possiamo usare un trucco, usando il currying *)

let arg x = fun y rest -> rest (op x y);;
let stop x = x;;
let f g = g init;;
(*  questa struttura contiene una serie di parametri non inizializzati, come init e op. 
	Dobbiamo definirli *)

let op = fun x y -> x + y;;
let init = 0;;

let arg x = fun y rest -> rest (op x y);;
let stop x = x;;
let f g = g init;;
f (arg 1) stop;;
f (arg 1) (arg 2) (arg 3) stop;; (* e cosi via *)

(* Da qua in poi non si ha capito nulla :P *)
(* Non sono però delle implementazioni parametriche. Possiamo tentare di renderle indipendenti
	o astrarrle ulteriormente, usando i Functors *)

module type OpVarADT =
	module Name : sig
		type a and b and c 
		val op: a -> b -> c
		val init : c
	end
(* a, b e c sopra sono tipi ignotici, ma non generici! Non so che tipo avranno,
	ma avranno un tipo sicuramente *)
module VarArgs (OP : OpVarADT.OpVarADT ) =
	struct
		let arg x = fun y rest -> rest (op x y);;
		let stop x = x;;
		let f g = g init;;
	end

(* Controllare gli slide dove ce ne sono esempi di uso dei funtori *)

(* Non è ancora perfetto però. Possiamo provare a instanziare OpVarADT con ua lista generica? *)
(* Dobbiamo fare un funtore di funtori *)