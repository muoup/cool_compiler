class Main {
    main() : Object {
        case self of
            m : Main => 0;
            o : Object => abort();
        esac
    };
};