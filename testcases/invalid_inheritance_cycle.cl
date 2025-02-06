class A inherits B{
	copy() : SELF_TYPE { self };
};

class B inherits A { };

class Main {
	x : B <- (new B).copy();
	main() : B { x };
};