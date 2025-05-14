The first part of our code generator converts the cl-type input into all the information we need. Large parts of the code to parse cl-type files
were taken from the semantic analyzer, and this process results in an AST, class map, parent class map, and implementation map. Next,
we generate TAC from this input. After some further cleaning and manipulation of the input data, the process of converting to TAC is 
fairly straightforward. The d_tac_data file describes all of the TAC expressions that we might generate, and includes all of the
necessary TAC data types. We use a symbol table to map TAC ids to the variables and expressions they refer to, and use this symbol table
to generate TAC for each class and each method inside each class. To generate individual TAC expressions, we match each expression in the
AST with its type (assignment, let with init, static/dynamic dispatch, etc.) and handle them as needed. This code is located mainly in the
e_tac_expr_gen file.

We suspect that a large difference in our code generator to others' is that we do not then convert the TAC to a control-flow graph, 
opting instead to go directly to assembly code. Although this may be against recommendations, we felt it was
more in the spirit of PA3 to first strive for simplicity and then implement the CFG when needed in PA4. This way, we could directly 
map TAC instructions to a corresponding set of assembly instructions without having to worry about basic blocks, as will be
described soon. Even though a CFG will almost certainly be necessary for PA4 (and we have already written some code for that), we 
felt it would only increase confusion to do so for an exceedingly unoptimized PA3.

Before converting the TAC to assembly, we have almost 500 lines of assembly that is common to every .s file we generate. This code, located in
the a_builtins file, does not depend on the contents of whatever COOL program code is being generated for. It includes 
the primitives used in the IO and string methods, all runtime error messages, and the assembly necessary to output them.
Although some parts of this file may not be required depending on the specific program being compiled (out_string is unnecessary if no
method inherits IO), this structure allows a clear delineation between the built-in functions and the true compiled program, simplifying
the .s file and the general process of generating assembly. Next, we generate a vtable, enabling dynamic dispatch. It is generated at 
the bottom of the .s file because we felt it was cleaner to have the methods start immediately after the built-in functions instead of 
being interrupted. The short g_metadata_output code is sufficient to generate all the methods for each class.

After these two necessary precursors, we can generate assembly code from TAC, which we strove to make as simple as possible. During the process,
we maintain a string map to let us get the string labels (made unique by maintaining a simple counter increased every time a new 
string is required) from strings in the program and a stack map, allowing us to access local variables and temporaries. Once we have this
data, we go through the TAC instructions and map each type to some assembly instructions, the code for which is located in the h_asm_gen file.
Our assembly code is purposely inefficient and simple - every value is immediately moved into memory, which allows us to eschew such 
complicated things as a register allocator. This also allows us to assume that no register ever contains a value we might like to retain,
so for simple instructions like arithmetic we can get the values from memory, move them into two random registers, and put the result back in 
memory. We also don't bother at this point with freeing memory, so we can call calloc to create objects without further complications. Since
we simply map TAC instructions one by one to assembly, no one instruction results in overly complex assembly.
Once we have our assembly instructions, the last thing to do is output them. The h_asm_data file has functions
that take our assembly types and print them in the correct format. It deals with the headers for each assembly string and function,
and prints out every other instruction as it should be.

Our four submitted test cases were chosen on the basis of being some of the most useful in discovering 
bugs in our compiler. The first test case confirms that we determine the types in case expressions correctly,
which was one of the last things we fixed. The variable x should be of its assigned type Derived, rather than its
stated type Base, and the case expression evaluates that. This test case also confirms we have some sort of working inheritance and
I/O. The second test case tests string I/O and operations in a loop that doesn't have a prescribed end condition, with its main use
being testing that we could give any number of inputs to the generated program and have it work correctly.

The third test case addresses one our biggest flaws with our initial test suite, namely that most of our test cases were not large
enough to generate enough data to reveal flaws in our logic. The code in this test case is somewhat nonsensical
if looked at closely, but it serves its purpose of touching almost all features of COOL, and thus almost all of
our code generator. Its voluminous output puts lots of data onto the stack and ensures that our code generator 
has to be pretty close to correct to pass this test case. The final test case is a simple dispatch on void error, which checks our general error 
handling code and that we can detect dispatches on void. It was chosen because it was probably the most complicated
runtime error to handle, excluding stack overflows, about which we have followed the reference compiler in ignoring entirely.