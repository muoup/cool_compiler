class OptionalInt {
	element : Int;

	set(i : Int) : OptionalInt {
		{
			element <- i;
			self;
		}
	};

	get() : Int {
		element	
	};
};

class IntSet {
	element : OptionalInt;
	next_element : IntSet;

	elem() : OptionalInt {
		element
	};
	
	next() : IntSet {
		next_element
	};

	append(i : Int) : Object {
		if (isvoid element) then
		{
			element <- (new OptionalInt).set(i);
			next_element <- new IntSet;
		}
		else 
			next_element.append(i)
		fi
	};

	remove(e : Int) : Object {
		if (isvoid element) then
			0
		else
			if (element.get() = e) then
			{
				element <- next_element.elem();
				next_element <- next_element.next();
			}
			else
				next_element.remove(e)
			fi
		fi
	};

	contains(e : Int) : Bool {
		if (element.get() = e) then
			true
		else
		{
			if (isvoid next_element) then
				false
			else
				next_element.contains(e)
			fi;
		}
		fi
	};

	length() : Int {
		if (isvoid element) then
			0
		else
			1 + next_element.length()
		fi
	};
};

class StringList {
	element : String;
	next_element : StringList;

	elem() : String {
		element
	};

	next() : StringList {
		next_element
	};

	find(e : String) : Int {
		if (element = "") then
			-- not in list
			~1
		else
		{
			if (e = element) then
				0
			else
				let find : Int <- next_element.find(e) in
				if find = ~1 then
					~1
				else
					1 + next_element.find(e)
				fi
			fi;
		}
		fi
	};

	append(str : String) : Object {
		if (element = "") then
		{
			element <- str;
			next_element <- new StringList;
		}
		else
			next_element.append(str)
		fi
	};

	nth(n : Int) : String {
		if (n = 0) then
			element
		else
			next_element.nth(n - 1)
		fi
	};

	length() : Int {
		if (element = "") then
			0
		else
			1 + next_element.length()
		fi
	};
};

class Task {
	id : Int;
	
	dependencies : IntSet;
	required_to	 : IntSet;

	init(task_id : Int) : Task {
		{
			id <- task_id;
			dependencies <- new IntSet;
			required_to <- new IntSet;
			self;
		}
	};

	id() : Int {
		id
	};

	deps() : IntSet {
		dependencies
	};

	req_to() : IntSet {
		required_to
	};
};

class TaskList {
	element : Task;
	next_element : TaskList;

	elem() : Task {
		element
	};

	next() : TaskList {
		next_element
	};

	append(task : Task) : Object {
		if (isvoid element) then
		{
			element <- task;
			next_element <- new TaskList;
		}
		else
			next_element.append(task)
		fi
	};

	length() : Int {
		if (isvoid element) then
			0
		else
			1 + next_element.length()
		fi
	};

	nth(i : Int) : Task {
		if (i = 0) then
			element
		else
			next_element.nth(i - 1)
		fi
	};
};

class State {
	id_contents : StringList;
	task_list	: TaskList;

	init() : State {
		{
			id_contents <- new StringList;
			task_list <- new TaskList;
			self;
		}
	};

	ids() : StringList {
		id_contents
	};

	tasks() : TaskList {
		task_list
	};
};

class Main inherits IO {
	gen_dep_list() : State {
		{
			let state : State <- (new State).init() in
			{
				let line_one : String <- in_string() in
				let line_two : String <- "" in
				{
					while (not (line_one = "")) loop
					{
						line_two <- in_string();

						let task_id : Int <-
							(
								let find : Int <- state.ids().find(line_one) in
								if (find = ~1) then
								{	
									state.tasks().append((new Task).init(state.ids().length()));
									state.ids().append(line_one);
									state.ids().length() - 1;
								}
								else
									find
								fi
							)
						in
						let dep_id : Int <-
							(
								let find : Int <- state.ids().find(line_two) in
								if (find = ~1) then
								{
									state.tasks().append((new Task).init(state.ids().length()));
									state.ids().append(line_two);
									state.ids().length() - 1;
								}
								else
									find
								fi
							)
						in
						{
							state.tasks().nth(dep_id).req_to().append(task_id);
							state.tasks().nth(task_id).deps().append(dep_id);
						};

						line_one <- in_string();
					}
					pool;
				};
				state;
			};
		}
	};

	first_alphabetically(state : State, id_list : IntSet) : Int {
		let acc_id : Int <- ~1 in
		let iter : IntSet <- id_list in
		let acc_str_equivalent : String <- "zzzzzzzzzzzzzzzzzzzz" in
		let iter_str_equivalent : String in
		{
			while (not (isvoid iter.elem())) loop
			{
				iter_str_equivalent <- state.ids().nth(iter.elem().get());

				if (iter_str_equivalent < acc_str_equivalent) then
				{
					acc_id <- iter.elem().get();
					acc_str_equivalent <- iter_str_equivalent;
				}
				else
					0
				fi;

				iter <- iter.next();
			}
			pool;
			
			acc_id;
		}
	};

	no_dependencies(state : State) : IntSet {
		{
			let no_deps : IntSet <- new IntSet in
			let iter : TaskList <- state.tasks() in
			{
				while (not (isvoid iter.elem())) loop
				{
					let task : Task <- iter.elem() in
					{
						if (isvoid task.deps().elem()) then
							no_deps.append(task.id())
						else
							0
						fi;
					};

					iter <- iter.next();
				}
				pool;	
				no_deps;
			};
		}
	};

	gen_order(state : State) : IntSet {
		{
			let order : IntSet <- new IntSet in
			{
				let no_deps : IntSet <- no_dependencies(state) in
				let next_elem : Int <- ~1 in
				let next_task : Task in
				while (not (isvoid no_deps.elem())) loop
				{
					next_elem <- first_alphabetically(state, no_deps);
					no_deps.remove(next_elem);

					let rm_dep_iter : TaskList <- state.tasks() in
					while (not (isvoid rm_dep_iter.elem())) loop
					{
						rm_dep_iter.elem().deps().remove(next_elem);
						rm_dep_iter <- rm_dep_iter.next();
					}
					pool;

					next_task <- state.tasks().nth(next_elem);					
					order.append(next_elem);

					let req_to_iter : IntSet <- next_task.req_to() in
					let task_equivalent : Task in
					{
						while (not (isvoid req_to_iter.elem())) loop
						{
							task_equivalent <- state.tasks().nth(req_to_iter.elem().get());

							-- if deps list is empty
							if (isvoid task_equivalent.deps().elem()) then
								no_deps.append(req_to_iter.elem().get())
							else
								0
							fi;
							req_to_iter <- req_to_iter.next();
						}
						pool;
					};
				}
				pool;

				if (order.length() = state.tasks().length()) then
					order
				else
					new IntSet
				fi;
			};
		}
	};

	print_string_list(lst: StringList) : Object {
		if (lst.elem() = "") then
			0
		else
		{
			out_string(lst.elem());
			out_string("\n");
			print_string_list(lst.next());
		}
		fi
	};
	
	print_task_list(lst: TaskList) : Object {
		if (isvoid lst.elem()) then
			0
		else
		{
			out_int(lst.elem().id());
			out_string("\n ");
			print_int_list(lst.elem().deps());
			out_string(" ");
			print_int_list(lst.elem().req_to());
			print_task_list(lst.next());
		}
		fi
	};

	print_int_list(lst: IntSet) : Object {
		if (isvoid lst.elem()) then
			out_string("\n")
		else
		{
			out_int(lst.elem().get());
			out_string(" ");
			print_int_list(lst.next());
		}
		fi
	};

	main() : Object {
		let state : State <- gen_dep_list() in
		let order : IntSet <- gen_order(state) in
		let order_iter : IntSet <- order in
		{
			if (isvoid order_iter.elem()) then
				out_string("cycle\n")
			else
				while (not (isvoid order_iter.elem())) loop
				{
					let task_name : String <- state.ids().nth(order_iter.elem().get()) in
					out_string(task_name);
					out_string("\n");
					
					order_iter <- order_iter.next();
				}
				pool
			fi;
		}
	};
};



