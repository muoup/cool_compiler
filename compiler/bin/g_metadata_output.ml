open A_util
open B_impl_map
open C_parser_data

let emit_vtable (output : string -> unit) (data : program_class_data) : unit =
    let vtable_name = vtable_name_gen data.name in 
    
    output @@ "\t.globl " ^ vtable_name;
    output "\n";
    output @@ "" ^ vtable_name ^ ":\n";

    List.iter (
        fun (_method : impl_method) -> 
            output "\t.quad\t";
            output @@ method_name_gen data.name _method.name;
            output "\n";
    ) data.methods;

    ()

let emit_obj_names (output : string -> unit) (data : program_class_data) : unit =
    let object_name = data.name in
    let header = obj_name_mem_gen object_name in

    output @@ "\t.globl " ^ header ^ "\n";
    output @@ "" ^ header ^ ":\n";
    output @@ "\t.string \"" ^ object_name ^ "\"\n"

let emit_metadata (output : string -> unit) (data : program_data) : unit =
    StringMap.iter (fun _ metadata -> emit_vtable output metadata) data.data_map;
    output "\n";
    StringMap.iter (fun _ metadata -> emit_obj_names output metadata) data.data_map
