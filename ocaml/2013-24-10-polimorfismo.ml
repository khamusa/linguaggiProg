let compose f g x = f (g x);;
(* val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun> *)

(* Non va fatta nessun'asunzione al tipo della funzione f, 
	della g o anche del parametro x, che potrebbe essere ancora un'altra funzione.
	L'unico vincolo definito e' che il tipo di uscita di gi sia lo stesso del tipo
	di 
	Posso convertire una funzione 'aperta' in una funzione 'chiusa'. *)

compose char_of_int int_of_char ;;
(* char -> char = <fun> *)

(* Funzione booleana not *)
compose (not) (not);;

compose (fun x -> x + 1) int_of_char;;

(* Ml usa il type inference per capire, non esattamnete quali sono i tipi coinvolti,
	ma quali sono le relazioni tra i tipi coinvolti *)

(* Cosa significa l'ad hoc polymorphism? è quello che fa java con l'overloading dei metodi
	cambiando soltanto i parametri, ad esempio. In pratica per ogni 'range' di valor 
	compattibili va dichiarata una nuova versione del costrutto. *)

(* Polimorfismo parametrico: scriviamo un unico codice, 'generico'. Nel caso di java sono
	i tipi genirici, quella roba tra < >. 'Generico' perché nocd 6n specifichiamo il tipo
	preciso su cui si deve lavorare  *)

(* Subtype Polymorphism: polimorphismo ottenuto tramite le relazioni di ereditarietà.
	Posso definire metodi in funzione di una superclasse, che ammette tutte le sue 
	sottoclassi. Applicazione del late binding. *)

(* WEAK-TYPED *)

let compose' = compose (fun c -> int_of_char c)
(* val compose' : ('_a -> char) -> '_a -> int = <fun> *)
(* Abbiamo composto la funzione compose, legando il primo parametro,
con una funzione che va da char -> int. char equivale a beta, e int equivale ad alfa *)

(* Il polimorfismo puro si legge come 'qualsiasi tipo che possa soddisfare quella funzione'. 

	'_a' invece significa che esiste uno o soltanto un tipo alfa che purò soddisfare, non significa
		però qualsiasi alfa *)

(* polymorphic adt stack *)

(* è un'implementazione con side effect, per quello si usa
	nel tipo dello stack un record con mutable *)
module Stack = struct
	type 'a stack = { mutable c : 'a list }
	exception EmptyStackException

	let empty () = { c = [] }
	(* se empty non fosse una funzione, avrebbe restituito sempre un record,
		che è quello che vogliamo, ma sempre LO STESSO RECORD, cioè,
		non potremo mai lavorare con questo modulo su più di uno stack diverso *)
	let push s x = s.c <- x :: s.c

	let pop s =
		match s.c with
		| hd :: tl -> s.c <- tl
		| [] -> raise EmptyStackException
end;;

#use "adtstack.ml";;


let s = Stack.empty();;
(* val s : '_a Stack.stack = {Stack.c = []} *)
Stack.push s 7;;
(* - : unit = () *)
Stack.push s 35;;
(* - : unit = () *)
s;;
(* - : int Stack.stack = {Stack.c = [35; 7]} *)

(* "zucchero" sintattico, per una funzione che ammette un parametro
	opzionale *)
let rec count ?(tot=0) x = function
	| [] -> tot
	| h :: l1 -> if (h == x) then count ~tot:(tot+1) x l1
							else count ~tot:tot x l1

(*  non è possibile fare pattern matching intelligente sulle stringhe
	iterare sulle stringhe va fatto, come nella funzione di libreria String.iter: *)
(* in ML non è obbligatorio che ci sia l'else *)
let rec iter f ?(k=0) s =
	if k < String.length s then ( f s.[k] ; iter f ~k:(k + 1) s);;

(* (>:) rappresenta la fuzione che usa ML dovrà usare per eseguire il 'minore di' *)
let qsort (>:) l =
		let rec qsort = function
		[] -> []
		| h::tl -> (qsort (List.filter (fun x -> (x >: h)) tl) )
			@ [h] @
			(qsort (List.filter (fun x-> (h >: x) ) tl))
		in qsort l

(* (>:) rappresenta la fuzione che usa ML dovrà usare per eseguire il 'minore di' *)
let qsort (>:) l =
		let rec qsort = function
		[] -> []
		| h::tl -> (qsort (List.filter (fun x -> (x >: h)) tl) )
			@ [h] @
			(qsort (List.filter (fun x-> (h >: x) ) tl))
		in qsort l
