let accept_all derivation string = Some (derivation, string)

type house_nonterminals =
  | House | Room | Hall | Lift | Auto | Manual

let house_grammar = 
  (House,
   function
     | House ->
        [[N Room; N Hall; N House];
	[N Room; N Lift; N House];
	[N Room]]
     | Room ->
	[[T"Bed"]; [T"Bath"]; [T"Living"]; [T"Kitchen"]; [T"Lounge"];
	[T"Closet"]; [T"Studio"]]
     | Hall ->
	[[T"Hallway"]; [T"Door"]; [T"Portal"]]
     | Lift ->
	[[N Auto]; [N Manual]]
     | Auto ->
	[[T"Elevator"]; [T"Jetpack"]]
     | Manual ->
	[[T"Ladder"]; [T"Stairs"]; [T"Trampoline"]])


let test_1 = parse_prefix house_grammar accept_all ["Hallway"; "Door"; "Lounge"; "Jetpack"] = None

let test_2 = parse_prefix house_grammar accept_all ["Living"; "Hallway"; "Kitchen"; "Door"; "Lounge"; "Jetpack"; "Bed"; "Ladder"; "Bath"; "Stairs"; "Studio"; "Elevator"; "Ladder"; "Closet"] = Some
 ([(House, [N Room; N Hall; N House]); (Room, [T "Living"]);
   (Hall, [T "Hallway"]); (House, [N Room; N Hall; N House]);
   (Room, [T "Kitchen"]); (Hall, [T "Door"]);
   (House, [N Room; N Lift; N House]); (Room, [T "Lounge"]);
   (Lift, [N Auto]); (Auto, [T "Jetpack"]);
   (House, [N Room; N Lift; N House]); (Room, [T "Bed"]); (Lift, [N Manual]);
   (Manual, [T "Ladder"]); (House, [N Room; N Lift; N House]);
   (Room, [T "Bath"]); (Lift, [N Manual]); (Manual, [T "Stairs"]);
   (House, [N Room]); (Room, [T "Studio"])],
  ["Elevator"; "Ladder"; "Closet"])
;;

