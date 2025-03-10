class O {
    overflow() : Object {
        let uh_oh : O <- new O in
        uh_oh.overflow()
    };
};

class Main {
	main() : Object {
        let o : O <- new O in
        o.overflow()
	};
};
