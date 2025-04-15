open A_util
open H_asm_data

let single_map (cmd : asm_cmd) : asm_cmd list =
    match cmd with
    | MOV   (src, dst) when src = dst -> []
    
    | ADD   (src, dst)
    | SUB   (src, dst) when src = IMMEDIATE 0 -> []
    
    | _ -> [cmd]

let double_map (cmd1 : asm_cmd) (cmd2 : asm_cmd) : asm_cmd list =
    match cmd1, cmd2 with
    | MOV   (src1, dst1), 
      MOV   (src2, dst2) when src1 = dst2 && dst1 = src2 ->
        [MOV (src1, dst1)]

    | MOV   (REG src1, dst1),
      MOV   (src2, dst2) when src2 = dst1 ->
        [
            MOV (REG src1, dst1);
            MOV (REG src1, dst2)
        ]

    | MOV   (src1, dst1),
      MOV   (src2, dst2) when dst1 = dst2 ->
        begin match src2 with
        | REG_offset _ -> [cmd1; cmd2]
        | _ -> [MOV (src2, dst2)]
        end

    | MOV   (REG_offset (reg1, off1), dst1),
      MOV   (REG_offset (reg2, off2), dst2) when reg1 = reg2 && off1 = off2 && reg1 <> RAX ->
        [
            MOV (REG_offset (reg1, off1), REG RAX);
            MOV (REG RAX, dst1);
            MOV (REG RAX, dst2);
        ]

    | MOV   (REG reg, REG_offset(reg1, off1)),
      MOV   (REG_offset(reg2, off2), dst) when reg1 = reg2 && off1 = off2 && reg1 <> RAX ->
        [
            MOV (REG reg, REG_offset(reg1, off1));
            MOV (REG reg, dst);
        ]

    | MOV   (REG_offset(reg1, off1), REG_offset(reg2, off2)),
      MOV   (REG_offset(reg3, off3), dst) when reg2 = reg3 && off2 = off3 && reg1 <> RAX ->
        [
            MOV (REG_offset(reg1, off1), REG RAX);
            MOV (REG RAX, REG_offset(reg2, off2));
            MOV (REG RAX, dst)
        ]
      
    | _ -> [cmd1; cmd2]

let peephold_optimize (_method : asm_method) : asm_method =
    let body = _method.commands
        |> List.map (single_map)
        |> List.concat
        |> sliding_window_2 (double_map)
    in

    { _method with commands = body }