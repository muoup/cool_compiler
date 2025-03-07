class Base {
	op(a : Derived, b : Derived) : Object { 0 };
};

class Derived inherits Base {
	op(a : Base, b : Base) : Object { 0 };
};

class Main {
	main() : Object { 0 };
};
