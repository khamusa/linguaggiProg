(*

Beyond the well-known Celsius and Fahrenheit, there are other six temperature scales: 
Kelvin, Rankine, Delisle, Newton, Réaumur, and Rømer 
(Look at http://en.wikipedia.org/wiki/Comparison_of_temperature_scales to read about them).

Write a function that given a pure number (????) prints a conversion table for it among any of the 8 scales 
(remember that functions are high-order).

Write a function that given a temperature in a specified scale returns a list of all the corresponding
temperatures in the other scales, note that the scale must be specified (hint: use a tuple).
*)

type scale = Celsius | Fahrenheit | Kelvin | Rankine | Delisle | Newton | Reaumur | Romer;;
let scales = [Celsius; Fahrenheit; Kelvin; Rankine; Delisle; Newton; Reaumur; Romer];;
exception IncorrentScale;;

let to_celsius (gradi, scale) =
	match scale with
	| Celsius 	-> (gradi, Celsius)
	| Fahrenheit -> ((gradi -. 32.) *. 5. /. 9., Celsius)
	| Kelvin 	-> (gradi -. 273.15, Celsius)
	| Rankine 	-> ((gradi -. 491.67) *. 5. /. 9., Celsius)
	| Delisle 	-> (100. -. gradi *. 2. /. 3., Celsius)
	| Newton 	-> (gradi *. 100. /. 33., Celsius)
	| Reaumur 	-> (gradi *. 5. /. 4., Celsius)
	| Romer 	-> ((gradi -. 7.5) *. 40. /. 21., Celsius);;

let from_celsius (gradi, el) target_scale =
	match el with
	| Celsius -> (match target_scale with
				| Celsius 	-> (gradi, Celsius)
				| Fahrenheit -> (gradi *. 9. /. 5. +. 32., Fahrenheit)
				| Kelvin 	-> (gradi +. 273.15, Kelvin)
				| Rankine 	-> ((gradi +. 273.15) *. 9. /. 5., Rankine)
				| Delisle 	-> ((100. -. gradi) *. 3. /. 2., Delisle)
				| Newton 	-> (gradi *. 33. /. 100., Newton)
				| Reaumur 	-> (gradi *. 4. /. 5., Reaumur)
				| Romer 	-> (gradi *. 21. /. 40. +. 7.5, Romer))
	| _ -> raise IncorrentScale;;

let convert el target = from_celsius (to_celsius el) target;;
let conversions el = List.map (convert el) scales;;

(* Usage *)
conversions (100., Celsius);;
conversions (75.9, Newton);;
conversions (184.0., Reaumur);;

