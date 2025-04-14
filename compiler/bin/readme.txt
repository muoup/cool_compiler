The first part of our code generator converts the cl-type input into all the information we needed. Large parts code to parse cl-type files
was taken from the semantic analyzer, and this process results in an AST, class map, parent class map, and implementation map. Next,
we generated TAC from this input. After some further cleaning and manipulation of the input data, the process of converting to TAC is 
fairly straightforward...

We suspect that a large difference in our code generator to others is that we do not convert the TAC to a control-flow graph, 
opting instead to go directly to assembly code. Although this is slightly against recommendations, the reason is that we felt it was
more important to strive for simplicity in PA3 and then implement the CFG when needed in PA4. This way, we could directly 
map TAC instructions to a corresponding set of assembly instructions without having to worry about basic blocks, as will be
described soon. Even though a CFG will almost certainly be necessary for PA4 (and we have already written some code for that), we 
felt it would be only increase confusion to do so for an exceedingly unoptimized PA3.

Builtins...

Vtable...

Describe tac to assembly...

Our four submitted test cases were chosen on the basis of being some of the most useful in discovering 
bugs in our compiler. The first test case confirms that we determine the types in case expressions correctly,
which was one of the last things we fixed. The variable x should be of its assigned type Derived, rather than its
stated type Base, and the case expression evaluates that. This test case also confirms we have some sort of working inheritance and
I/O. The second test case...