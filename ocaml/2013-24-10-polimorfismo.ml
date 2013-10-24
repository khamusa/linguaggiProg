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
	i tipi genirici, quella roba tra < >. 'Generico' perché non specifichiamo il tipo
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

