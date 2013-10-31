(* Graph Coverage *)
(* Depth firs search in un grafo orientato. Fare l'albero di copertura dell'oggetto partendo
da un nodo che verrà dato. *)
(* Grafi aciclici orientati *)

(* Abbiamo bisogno di un albero per rappresentare l'albero di copertura *)
type 'a tree = Leaf of 'a | Tree of ('a * 'a tree list);; (* struttura dati polimorfica *)

(* un tipo di dato per il grafo *)

module type GraphADT =
	sig
		type 'a graph
		... slide
	end

module Graph : GraphADT =
	struct
		type 'a graph = Graph of ( 'a list ) * ( ('a * 'a) list )

		let empty() = Graph([], [])
		
		(* Se l'insieme dei nodi è vuoto, il grafo è vuoto *)
		let is_empty = function
			Graph(nodes, _) -> (nodes = [])

		exception TheGraphIsEmpty
		exception TheNodeIsNotInGraph

		(* AUSILIARE: check se un 'a e' nella 'a list *)
		let rec is_in_list ?(res=false) x = function
			[]	-> res
			| h::tl	-> is_in_list ~res: (res || (x=h)) x tl
		end


		let node_is_in_graph n = function
			Graph (nodes, _)	-> is_in_list n nodes

		(* Aggiungiamo un elemento in una lista, ma SOLO se non è presente *)
		let rec add_in_list ?(res=[]) x = function
			[] -> List.rev x::res
			| h::tl when (h=x)
			| ...

		let add_node n = function
			| Graph(nodes, arcs)	-> Graph((add_in_list n nodes), arcs)

		let add_arc s d = function
			Graph(nodes, arcs) ->
				Graph( (add_in_list d (add_in_list s nodes)), (add_in_list (s,d) arcs ) )

		(* fst and snd sono funzioni su coppie *)
		let adjacents n =
			let adjacents n l = List.map snd (List.filter (fun x -> ((fst x) = n)) l)
		in function
			Graph(_, arcs) -> adjacents n arcs

		end

(*  uso del grafo *)
open Graph

(* Trasforma una lista di archi in un grafo *)
let arcs_to_graph arcs = 
	let rec arcs_to_graph g = function
	[] -> g
	| (s, d) :: tl -> arcs_to_graph (add_arc s d g) tl
in arcs_to_graph (empty()) arcs

(* Dato un grafo aciclico, estrae un'albero *)

let graph_to_tree g root = 
	...

let dfs g v =
	....