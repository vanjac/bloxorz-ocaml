#load "str.cma"

let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let is_data_line (line:string) = match line with
    | "" -> false
    | _ -> not (String.contains line '<')

let num_to_tile num_str = match num_str with
    | "0" -> "Hole"
    | "1" -> "Floor"
    | "2" -> "Thin"
    | "3" -> "Finish"
    | _ -> failwith "unrecognized tile " ^ num_str

let rec nums_to_tiles num_strs = match num_strs with
    | [] -> ""
    | head::tail -> num_to_tile head ^ ";" ^ (nums_to_tiles tail)

let data_line_to_tile_line data_line =
    "[" ^ (nums_to_tiles (Str.split (Str.regexp ",") data_line)) ^ "];\n"

let rec tmx_to_board tmx = match tmx with
    | [] -> ""
    | head::tail -> if is_data_line head
        then (tmx_to_board tail) ^ (data_line_to_tile_line head)
        else tmx_to_board tail

let convert_tmx filename =
    print_string ( "[\n" ^ tmx_to_board (read_file filename) ^ "]\n")

;;
convert_tmx (Sys.argv.(1));;
