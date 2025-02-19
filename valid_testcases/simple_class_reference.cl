class Point {
	x : Int;
	y : Int;

	init(new_x : Int, new_y : Int) : SELF_TYPE {
		{
			x <- new_x;
			y <- new_y;
			self;
		}
	};

	get_x() : Int { x };

	get_y() : Int { y };
};

class Main inherits IO {
	main() : Object {
		{
			let point : Point <- (new Point).init(1, 1) in
			out_int(point.get_x());

			out_string("\n");
		}
	};
};
