(*
Put into a list, called alkaline_earth_metals, 
the atomic numbers of the six alkaline earth metals: 
beryllium (4), magnesium (12), calcium (20), strontium (38), barium (56), and radium (88). Then

Write a function that returns the highest atomic number in alkaline_earth_metals.
Write a function that sorts alkaline_earth_metals in ascending order 
(from the lightest to the heaviest).

*)

let alkaline_earth_metals = [38; 4; 56; 12; 88; 20];;

exception ListaVuota;;
let rec higher = function
	| hd :: [] -> hd
	| hd :: tail -> max hd (higher tail)
	| [] -> raise ListaVuota ;;


(*
Write a function that sorts alkaline_earth_metals in ascending order (from the lightest to the heaviest).
*)
let rec sort lst =	
	let rec insert ls x =
		match ls with
		| hd::tail -> if hd > x then x :: hd :: tail else hd :: (insert tail x)
		| [] -> [x]
	in
	(match lst with
		| hd :: [] -> [hd]
		| hd :: tail -> insert (sort tail) hd
		| [] -> []);;


(*
Put into a second list, called noble_gases, the noble gases: 
helium (2), neon (10), argon (18), krypton (36), xenon (54), and radon (86). 
Then Write a function (or a group of functions) that merges the two lists and
print the result as couples (name, atomic number) sorted in ascending order on the element names.
*)

let noble_gases = [10; 2; 18; 36; 86; 54];;
let aem = alkaline_earth_metals;;

exception UnknownElement;;

let el_name = function
	| 2 -> "helium"
	| 4 -> "beryllium"
	| 10 -> "neon"
	| 12 -> "magnesium"
	| 18 -> "argon"
	| 20 -> "calcium"
	| 36 -> "krypton"
	| 38 -> "strontium"
	| 54 -> "xenon"
	| 56 -> "barium"
	| 86 -> "radon"
	| 88 -> "radium"
	| _ -> raise UnknownElement;;
	

(* ridefinisco la insert e la sort in forma parametrica *)
let rec insert_p f ls x =
		match ls with
		| hd::tail -> if (f hd x) then x :: hd :: tail else hd :: (insert_p f tail x)
		| [] -> [x];;

let rec sort_p f lst =	
	match lst with
	| hd :: [] -> [hd]
	| hd :: tail -> insert_p f (sort_p f tail) hd
	| [] -> [];;

(* lavoro direttamente sulle coppie, uso la tuplenize prima per trasformare la lista di numeri atomici *)
let tuplenize = List.map (fun nr -> (nr, el_name nr));; (* Trasforma una lista di interi di numeri atomici in una lista di coppie (nr_atomico, nome) *)
(* dichiaro funzioni ausiliari per il lavoro di ordinamento sui nomi delle coppie *)
let sort_by_tuple = sort_p (fun (_, el_name1) (_, el_name2) -> el_name1 > el_name2);; 
let insert_by_tuple = insert_p (fun (_, el_name1) (_, el_name2) -> el_name1 > el_name2);;

(** merge two (el_nr, el_name) lists into one big ordered_by_el_name list **)
let rec merge ls1 ls2 =
	let ls1b = sort_by_tuple (tuplenize ls1)
	and ls2b = sort_by_tuple (tuplenize ls2)
	in (
		let rec merge_lists ls1b ls2b =
		match ls1b with
		| [] -> ls2b
		| el :: tail -> merge_lists tail (insert_by_tuple ls2b el) in merge_lists ls1b ls2b
	);;

merge aem noble_gases;;




