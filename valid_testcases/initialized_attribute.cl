class Main {
	num : Int <- 5;
	num_complex : Int <- 2 * (~3);

	comp 	: Bool <- 3 < 2;
	compI 	: Bool <- 3 <= 2;
	compII	: Bool <- 3 = 2;

	str		: String <- "Hello, world!";

	block_	: Int <- 
		let val : Int <- 5 in
		2 + val
	;

	main() : Object { 0 };
};
