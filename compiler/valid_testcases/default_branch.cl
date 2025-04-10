class Main inherits IO {
    main() : Object {
        case 1 of
            s : String => abort();
            o : Object => out_string("Object");
        esac
    };
};