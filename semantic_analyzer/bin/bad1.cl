class A{
	copy1() : SELF_TYPE { self };
};

class B inherits A { 
	copy2() : SELF_TYPE { copy1() };
};

class C inherits B{ 
	copy3() : SELF_TYPE { let x : Object <-(new B)@A.copy2() in self};
};

class Main {
	x : B <- (new B).copy();
	main() : B { x };
};