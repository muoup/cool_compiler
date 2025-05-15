The first step of our optimizations is to construct a control flow graph, since we did not do that for PA3.
This makes our general pipeline: cl-type -> TAC -> CFG -> Optimized CFG -> Assembly -> Optimized Assembly.
We define the whole CFG of the program as a list of CFGs, one for each method. Since we didn't do any
whole-program analysis (although maybe graph coloring can get tackled over the summer), this representation
works perfectly well. The CFG for each method has a hashtable of basic blocks, as well as some other metadata
used for finding/replacing/updating/optimizing the CFG for each method. A basic block contains an id, label, 
some metadata, and the instructions in the basic block, as well as a list of references to the blocks successors
and predecessors. This is used for dead code elimination, since if a block has no successors and predecessors it can
be removed without causing any negative effects.

Once our control flow graph has been created, we do dead code elimination on it, which consists of two main steps.
First, we perform dead code elimination on the CFG of each method. We used the dataflow analysis method as shown
in class to accomplish this, using live_out, live_in, gen, and kill sets. Some instruction types are critical
and can't be removed (method calls, dispatch, etc.), but everything else is only kept if our analysis says it's 
necessary. After we run dead code elimination, we have an optimized set of methods with TAC instructions.

Next, we convert these TAC instructions to assembly. This code has been changed since PA3, and the largest 
difference is that we have a register allocation strategy that isn't "put everything in memory and worry about
it later". Therefore, we have updated our "where is this piece of data located" function, which we call
get_symbol_storage. REGISTER ALLOCATION STUFF HERE

Once some somewhat optimized assembly code has been generated, we then apply some peephole optimizations to it.
These optimizations consist of algebraic simplifications, changing some instructions to faster ones (e.g. 
mult -> add), and removing unnecessary move instructions. We identify what instructions to remove by looking at
them in isolation to see if they are wholly unnecessary, like adding 0 or multiplying by 1, and in pairs. 