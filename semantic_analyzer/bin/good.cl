class Main inherits IO{
	main() : Object {
		let b : Bool <- prompt() in
		while not b loop 
			b <- prompt() 
		pool
	};

	prompt() : Bool {
		{
		out_string("Speak, friend, and enter:\n");
		let x : String <- in_string() in
		if x = "friend" then true else false fi;
		}
   };
};
