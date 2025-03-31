module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

let method_name_gen (class_name : string) (method_name : string) =
  class_name ^ "." ^ method_name

let vtable_name_gen (class_name : string) =
  ".vtable_" ^ class_name