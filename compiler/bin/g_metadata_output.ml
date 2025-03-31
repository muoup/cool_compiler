open A_format
open F_metadata

let emit_vtable (output : string -> unit) (metadata : class_metadata) : unit =
    let vtable_name = vtable_name_gen metadata.class_name in 
    
    output @@ "\t.globl " ^ vtable_name;
    output "\n";
    output @@ "\t" ^ vtable_name;
    output ":\n";

    List.iter (
        fun _method_name -> 
        output ".quad\t";
        output @@ method_name_gen metadata.class_name _method_name;
        output "\n";
    ) metadata.method_table;

    ()

let emit_metadata (output : string -> unit) (metadata : metadata) : unit =
    StringMap.iter (fun _ metadata -> emit_vtable output metadata) metadata.class_metadata
