class Main inherits IO {
    val (obj : Object) : Object {
        case obj of
            i : Int => out_string("Success!\n");
            o : Object => abort();
        esac
    };

    main() : Object {
        val(2)
    };
};