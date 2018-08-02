(* Types *)

type tile =
    | Hole
    | Floor
    | Finish
    | Thin

type orientation =
    | NorthSouth (* coordinate is North *)
    | EastWest (* coordinate is West *)
    | Upright

type direction =
    | North
    | East
    | South
    | West

type game_state = { board: tile list list; orient:orientation; pos:int*int }

(* Levels *)

let level1 = {
    (*board is mirrored vertically*)
    board=[
[Hole;Hole;Hole;Hole;Hole;Hole;Floor;Floor;Floor;Hole;];
[Hole;Hole;Hole;Hole;Hole;Floor;Floor;Finish;Floor;Floor;];
[Hole;Floor;Floor;Floor;Floor;Floor;Floor;Floor;Floor;Floor;];
[Floor;Floor;Floor;Floor;Floor;Floor;Floor;Floor;Floor;Hole;];
[Floor;Floor;Floor;Floor;Floor;Floor;Hole;Hole;Hole;Hole;];
[Floor;Floor;Floor;Hole;Hole;Hole;Hole;Hole;Hole;Hole;];
];
    orient=Upright; pos=(1,4)
}

let level3 = {
    board=[
[Hole;Hole;Hole;Hole;Hole;Hole;Hole;Hole;Hole;Hole;Hole;Hole;Floor;Floor;Floor;];
[Floor;Floor;Floor;Floor;Hole;Hole;Hole;Hole;Hole;Hole;Hole;Floor;Floor;Floor;Floor;];
[Floor;Floor;Floor;Floor;Hole;Hole;Hole;Hole;Hole;Hole;Hole;Floor;Floor;Finish;Floor;];
[Floor;Floor;Floor;Floor;Floor;Floor;Floor;Floor;Floor;Hole;Hole;Floor;Floor;Floor;Floor;];
[Floor;Floor;Floor;Floor;Hole;Hole;Floor;Floor;Floor;Hole;Hole;Floor;Floor;Hole;Hole;];
[Hole;Hole;Hole;Hole;Hole;Hole;Floor;Floor;Floor;Floor;Floor;Floor;Floor;Hole;Hole;];
];
    orient=Upright; pos=(1,2)
}

let level4 = {
    board=[
[Hole;Hole;Hole;Hole;Hole;Floor;Floor;Floor;Hole;Hole;Thin;Thin;Thin;Thin;];
[Hole;Hole;Hole;Hole;Hole;Floor;Finish;Floor;Hole;Hole;Thin;Thin;Floor;Thin;];
[Floor;Floor;Floor;Hole;Hole;Floor;Floor;Floor;Floor;Thin;Thin;Thin;Thin;Thin;];
[Floor;Floor;Floor;Hole;Hole;Floor;Floor;Floor;Floor;Thin;Thin;Thin;Thin;Thin;];
[Floor;Floor;Floor;Hole;Hole;Hole;Hole;Hole;Hole;Hole;Floor;Floor;Hole;Hole;];
[Floor;Floor;Floor;Hole;Hole;Hole;Hole;Hole;Hole;Hole;Floor;Floor;Hole;Hole;];
[Floor;Floor;Floor;Floor;Hole;Hole;Hole;Hole;Hole;Floor;Floor;Floor;Hole;Hole;];
[Hole;Hole;Hole;Thin;Thin;Thin;Thin;Thin;Thin;Thin;Hole;Hole;Hole;Hole;];
[Hole;Hole;Hole;Thin;Thin;Thin;Thin;Thin;Thin;Thin;Hole;Hole;Hole;Hole;];
];
    orient=Upright; pos=(1,3)
}

let level6 = {
    board=[
[Hole;Hole;Hole;Hole;Hole;Hole;Hole;Floor;Floor;Floor;Hole;Hole;Hole;Hole;Hole;];
[Hole;Hole;Hole;Hole;Hole;Hole;Floor;Floor;Floor;Floor;Floor;Hole;Hole;Hole;Hole;];
[Hole;Hole;Hole;Hole;Hole;Hole;Floor;Floor;Floor;Floor;Floor;Hole;Hole;Hole;Hole;];
[Hole;Hole;Hole;Hole;Hole;Hole;Floor;Hole;Hole;Floor;Floor;Hole;Hole;Hole;Hole;];
[Hole;Hole;Hole;Hole;Floor;Floor;Floor;Hole;Hole;Hole;Hole;Hole;Floor;Floor;Floor;];
[Hole;Hole;Hole;Hole;Floor;Floor;Floor;Hole;Hole;Hole;Hole;Floor;Floor;Finish;Floor;];
[Floor;Floor;Floor;Floor;Floor;Floor;Hole;Hole;Hole;Hole;Hole;Floor;Floor;Floor;Floor;];
[Hole;Hole;Hole;Hole;Hole;Floor;Hole;Hole;Floor;Floor;Floor;Floor;Floor;Hole;Hole;];
[Hole;Hole;Hole;Hole;Hole;Floor;Hole;Hole;Floor;Floor;Floor;Hole;Hole;Hole;Hole;];
[Hole;Hole;Hole;Hole;Hole;Floor;Floor;Floor;Floor;Floor;Floor;Hole;Hole;Hole;Hole;];
];
    orient=Upright; pos=(0,6)
}

(* Printing Functions *)

let tile_to_string tile = match tile with
    | Hole -> " "
    | Floor -> "X"
    | Finish -> "U"
    | Thin -> "T"

let block_is_at_origin orient (x, y) =
    if x = 0 && y = 0 then true
    else if orient = NorthSouth && x = 0 && y = 1 then true
    else if orient = EastWest && x = -1 && y = 0 then true
    else false

let rec row_to_string row orient (x, y) = match row with
    | [] -> ""
    | head::tail ->
        (if (block_is_at_origin orient (x,y)) then "B" else tile_to_string head)
        ^ " "
        ^ (row_to_string tail orient (x - 1, y))

let rec game_to_string {board; orient; pos=(x, y)} = match board with
    | [] -> ""
    | head::tail ->
        (game_to_string {board=tail; orient=orient; pos=(x, y - 1)})
        ^ "\n" ^ " " ^ (row_to_string head orient (x,y))

let print_game game = print_string (game_to_string game)

(* Game Logic *)

let move game_state c =
   let (x,y) = game_state.pos in
   match c with
       | North ->
           (match game_state.orient with
           | Upright -> {board = game_state.board; orient = NorthSouth; pos = (x,y+2)}
           | NorthSouth -> {board = game_state.board; orient = Upright; pos = (x,y+1)}
           | EastWest -> {board = game_state.board; orient = EastWest; pos = (x,y+1)}
           )
       | South ->
           (match game_state.orient with
           | Upright -> {board = game_state.board; orient = NorthSouth; pos = (x,y-1)}
           | NorthSouth -> {board = game_state.board; orient = Upright; pos = (x,y-2)}
           | EastWest -> {board = game_state.board; orient = EastWest; pos = (x,y-1)}
           )
       | East ->
           (match game_state.orient with
           | Upright -> {board = game_state.board; orient = EastWest; pos = (x+1,y)}
           | NorthSouth -> {board = game_state.board; orient = NorthSouth; pos = (x+1,y)}
           | EastWest -> {board = game_state.board; orient = Upright; pos = (x+2,y)}
           )
       | West ->
           (match game_state.orient with
           | Upright -> {board = game_state.board; orient = EastWest; pos = (x-2,y)}
           | NorthSouth -> {board = game_state.board; orient = NorthSouth; pos = (x-1,y)}
           | EastWest -> {board = game_state.board; orient = Upright; pos = (x-1,y)}
           )


let tile_at {board; orient; pos=(x,y)} =
    if y>=(List.length board)||y<0 ||x<0||x>=List.length (List.nth board 0)then Hole else
    let row  = List.nth board y in
    let tile = List.nth row x in tile

let rec is_valid' {board;orient; pos=(x,y)} (secondary:bool)=
    match orient with
    |EastWest->
    let tile = tile_at {board;orient; pos=(x,y)} in
        if tile == Hole then false
        else is_valid' {board;orient=Upright;pos=(x+1,y)} true
    |NorthSouth->
    let tile = tile_at {board;orient; pos=(x,y)} in
        if tile == Hole then false
        else is_valid' {board;orient=Upright;pos=(x,y-1)} true
    |Upright ->
    let tile = tile_at {board;orient; pos=(x,y)} in
        if tile == Hole then false else if tile== Thin && secondary==false then false
        else true

let is_valid game_state=is_valid' game_state false

let is_winning {board;orient;pos=(x,y)}=
    let tile = tile_at {board;orient;pos=(x,y)} in
    if orient == Upright && tile == Finish then true else false

(* Game Interface *)

type level_exit =
    | Win
    | Lose
    | Quit

(* Return true if win *)
let rec level_loop game_state =
    if not (is_valid game_state) then
    (print_string "You fell! Try again...\n"; Lose)
    else if is_winning game_state then
    (print_string "You win!\n"; Win)
    else
    (print_game game_state; print_newline ();
    match read_line ()  with
        | "w" -> level_loop (move game_state North)
        | "a" -> level_loop (move game_state West)
        | "s" -> level_loop (move game_state South)
        | "d" -> level_loop (move game_state East)
        | "q" -> Quit
        | _ -> print_string "Invalid command"; level_loop game_state)

let rec game_loop levels level_num = match levels with
    | [] -> print_string "You completed the game!\n"
    | head::tail -> (print_string ("STAGE " ^ (string_of_int level_num) ^ "\n");
        match level_loop head with
        | Win -> game_loop tail (level_num + 1)
        | Lose -> game_loop levels level_num
        | Quit -> ())

;;

print_string "BLOXORZ\nPress Enter to start the game...\n";;
read_line ();;
game_loop [level1;level3;level4;level6] 1;;
