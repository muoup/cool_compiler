class Base {
	init() : Object { 0 };
};

class Derived inherits Base {
	init() : Object {
		self@Base.init()
	};
};

class Main {
	main() : Object { 0 };
};
