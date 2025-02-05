class Main inherits IO {
	get_number() : Object {
		5
	};

	main() : Object {
		let num : Int <- get_number() in
		out_int(num)
	};
};
