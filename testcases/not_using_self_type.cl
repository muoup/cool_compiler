class Silly {
	copy() : Silly { self };
};

class Sally inherits Silly { };

class Main {
	x : Sally <- (new Sally).copy();

	main() : Sally { x };
};