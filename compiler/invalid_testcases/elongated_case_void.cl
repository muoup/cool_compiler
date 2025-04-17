class Identity {
    identity(x : Identity) : Object {
        let o : Object in
        o
    };
};

class Main {
    main() : Object {
        let i : Identity <- new Identity in 

        case
            i
            .
            identity(i)
            of 
            o : Object => 0;
        esac
    };
};