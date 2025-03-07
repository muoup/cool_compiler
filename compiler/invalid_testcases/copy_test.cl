class IntWrapper {
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
        let wrapper1 : IntWrapper <- (new IntWrapper).init(1) in
        {
            let wrapper2 : IntWrapper <- wrapper1.copy() in
            {
                wrapper2.init(2);

                if wrapper1.getI() = wrapper2.getI() then
                    abort()
                else
                    0
                fi;  
            };   
        }
    };
};