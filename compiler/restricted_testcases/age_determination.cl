class Main inherits IO {
    main() : Object {
        let age : Int <- in_int() in

        if age < 18 then
            out_string("Age < 18\n")
        else if age <= 30 then
            out_string("Age <= 30\n")
        else if not (age <= 30) then
            out_string("Age > 30\n")
        else
            0
        fi fi fi
    };
};