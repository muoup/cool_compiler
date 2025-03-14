open G_tac_data
open Printf

type basic_block = {
  label: string option;
  instructions: tac_cmd list;
  successors: string list; 
}

type cfg = (string, basic_block) Hashtbl.t

let string_of_tac_cmd = function
  | TAC_add (dst, src1, src2) -> printf "  %s = %s + %s" dst src1 src2
  | TAC_sub (dst, src1, src2) -> printf "  %s = %s - %s" dst src1 src2
  | TAC_mul (dst, src1, src2) -> printf "  %s = %s * %s" dst src1 src2
  | TAC_div (dst, src1, src2) -> printf "  %s = %s / %s" dst src1 src2
  | TAC_lt (dst, src1, src2)  -> printf "  %s = (%s < %s)" dst src1 src2
  | TAC_lte (dst, src1, src2) -> printf "  %s = (%s <= %s)" dst src1 src2
  | TAC_eq (dst, src1, src2)  -> printf "  %s = (%s == %s)" dst src1 src2
  | TAC_int (dst, n) -> printf "  %s = %d" dst n
  | TAC_str (dst, s) -> printf "  %s = \"%s\"" dst s
  | TAC_bool (dst, b) -> printf "  %s = %b" dst b
  | TAC_ident (dst, id) -> printf "  %s = %s" dst id
  | TAC_lnot (dst, src) -> printf "  %s = !%s" dst src
  | TAC_bnot (dst, src) -> printf "  %s = ~%s" dst src
  | TAC_new (dst, typ) -> printf "  %s = new %s" dst typ
  | TAC_default (dst, typ) -> printf "  %s = default %s" dst typ
  | TAC_isvoid (dst, src) -> printf "  %s = isvoid %s" dst src
  | TAC_call (dst, func, args) -> printf "  %s = call %s(%s)" dst func (String.concat ", " args)
  | TAC_label lbl -> printf "%s:" lbl
  | TAC_jmp lbl -> printf "  goto %s" lbl
  | TAC_bt (cond, lbl) -> printf "  if %s then goto %s" cond lbl
  | TAC_return v -> printf "  return %s" v
  | TAC_comment c -> printf "  ; %s" c

let print_cfg (cfg : cfg) =
  Hashtbl.iter (fun lbl block ->
    printf "Block: %s\n" (Option.value ~default:"<entry>" block.label);
    List.iter (fun instr -> (string_of_tac_cmd instr; print_newline (); )) block.instructions;
    printf "  Successors: [%s]\n\n" (String.concat ", " block.successors)
  ) cfg
  

let build_cfg (tac_list : tac_cmd list) : cfg =
  let cfg = Hashtbl.create 10 in
  let current_block = ref { label = None; instructions = []; successors = [] } in
  let label_map = Hashtbl.create 10 in

  let add_block () =
    match !current_block.label with
    | Some lbl -> Hashtbl.add cfg lbl !current_block
    | None -> ()
  in

  let rec process cmds =
    match cmds with
    | [] -> add_block ()
    | TAC_label lbl :: rest ->
      add_block ();
      current_block := { label = Some lbl; instructions = []; successors = [] };
      Hashtbl.add label_map lbl !current_block;
      process rest
    | TAC_jmp lbl :: rest ->
      current_block := { !current_block with instructions = !current_block.instructions @ [TAC_jmp lbl]; successors = [lbl] };
      add_block (); process rest
    | TAC_bt (cond, lbl) :: rest ->
      current_block := { !current_block with instructions = !current_block.instructions @ [TAC_bt (cond, lbl)]; successors = [lbl] };
      add_block (); process rest
    | TAC_return _ as ret :: rest ->
      current_block := { !current_block with instructions = !current_block.instructions @ [ret]; successors = [] };
      add_block (); process rest
    | TAC_comment _ :: rest ->
        process rest
    | instr :: rest ->
      current_block := { !current_block with instructions = !current_block.instructions @ [instr] };
      process rest
  in

  process tac_list;

  (* Resolve fall-through edges *)
  Hashtbl.iter (fun lbl block ->
    match block.instructions with
    | TAC_jmp _ :: _ | TAC_return _ :: _ -> ()
    | _ ->
      let next_block = Hashtbl.find_opt label_map lbl in
      match next_block with
      | Some next -> Hashtbl.replace cfg lbl { block with successors = block.successors @ [next.label |> Option.get] }
      | None -> ()
  ) cfg;

  cfg
