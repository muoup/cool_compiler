open A_parser
open D_tac_data

let cmds_to_method (lst : tac_cmd list) : method_tac list = 
  [{
  class_name = "Main";
  method_name = "Main.main";
  arg_count = 0;
  commands = lst;
  ids = []
  }]

let parse_line (data : parser_data) : (parser_data * string) =
  let val_ = input_line data.file_handle in

  { data with line_number = data.line_number + 1 }, val_

let parse_id (s : string) : tac_id =
  if String.length s >= 2 && String.sub s 0 2 = "t$" then
    let number_str = String.sub s 2 (String.length s - 2) in
    let number = int_of_string number_str in
    Temporary number
  else
    failwith ("Invalid tac_id: " ^ s)
    
let parse_tac_line (data : parser_data) (line : string) : (parser_data * tac_cmd) =
  let tokens = String.split_on_char ' ' (String.trim line) |> List.filter (fun s -> s <> "") in
  match tokens with
  | [x; "<-"; "+"; y; z] -> data, TAC_add (parse_id x, parse_id y, parse_id z)
  | [x; "<-"; "-"; y; z] -> data, TAC_sub (parse_id x, parse_id y, parse_id z)
  | [x; "<-"; "*"; y; z] -> data, TAC_mul (parse_id x, parse_id y, parse_id z)
  | [x; "<-"; "/"; y; z] -> data, TAC_div (data.line_number, parse_id x, parse_id y, parse_id z)

  | [x; "<-"; "<"; y; z] -> data, TAC_lt  (parse_id x, parse_id y, parse_id z)
  | [x; "<-"; "<="; y; z] -> data, TAC_lte (parse_id x, parse_id y, parse_id z)
  | [x; "<-"; "="; y; z] -> data, TAC_eq  (parse_id x, parse_id y, parse_id z)

  | [x; "<-"; "int"; int_lit] -> data, TAC_int (parse_id x, int_of_string int_lit)
  | [x; "<-"; "bool"; bool_lit] -> data, TAC_bool (parse_id x, bool_of_string bool_lit)
  | [x; "<-"; "string"] ->
      let data', string_content = parse_line data in
      data', TAC_str (parse_id x, string_content)

  | [x; "<-"; "~"; y] -> data, TAC_neg (parse_id x, parse_id y)
  | [x; "<-"; "not"; y] -> data, TAC_not (parse_id x, parse_id y)

  | [x; "<-"; "new"; typ] -> data, TAC_new (parse_id x, typ)
  | [x; "<-"; "default"; typ] -> data, TAC_default (parse_id x, typ)

  | ["jmp"; label] -> data, TAC_jmp label
  | ["label"; label] -> data, TAC_label label
  | ["return"; x] -> data, TAC_return (parse_id x)
  | ["bt"; x; label] -> data, TAC_bt (parse_id x, label)

  | x :: "<-" :: "call" :: func :: args ->
      data, TAC_call (parse_id x, func, List.map parse_id args)

  | "comment" :: rest ->
      let comment_text = String.concat " " rest in
      data, TAC_comment comment_text
  | _ ->
      failwith ("Cannot parse line: " ^ line)

    
let rec parse_tac_file (data : parser_data) : tac_cmd list =
  try
    let (data', line) = parse_line data in
    let (data'', cmd) = parse_tac_line data' line in
    cmd :: parse_tac_file data''
  with End_of_file ->
    []
      
      