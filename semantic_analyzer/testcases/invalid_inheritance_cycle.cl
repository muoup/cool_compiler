class A inherits C{
	copy() : SELF_TYPE { self };
};

class B inherits A { };

class C inherits B{ };

class Main {
	x : B <- (new B).copy();
	main() : B { x };
};