class A{
    y : Int <- 3;
	copy() : SELF_TYPE { self };
};

class B inherits A { 
    y : Int <- 4;
};

class Main {
	main() : B { x };
};