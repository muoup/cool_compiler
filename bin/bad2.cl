class TestClass {
	i : Int;

	init(o : Object) : TestClass {
		{
			i <- o;
			self;
		}
	};
};

class Main {
	main() : Object {
		(new TestClass).init(1)
	};
};
