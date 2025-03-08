class Data {
    i : Int;

    init(ii : Int) : SELF_TYPE {
        {
            i <- ii;
            self;
        }
    };

    getI() : Int {
        i
    };
};

class Main {
    main() : Object {
        let data1 : Data <- new Data, data2 : Data <- data1 in
        {
            data1.init(5);

            if data1.getI() = data2.getI() then
                0
            else
                abort()
            fi;   
        }
    };
};