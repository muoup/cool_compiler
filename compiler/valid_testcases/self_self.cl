class Main {

    st : SELF_TYPE;

	func() : Object { self };

    gen_self() : SELF_TYPE { new SELF_TYPE };
	
	main() : Object { 
        let f : Object <- func() in
        let s : Object <- st in 
        gen_self()
     };
};
