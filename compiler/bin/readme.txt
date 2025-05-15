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
get_symbol_storage. What we implemented is probably better thought of as a triage, as it was not a full
register allocation strategy, but a mish-mosh of different ways to minimize the amount of moves we have
to do between different storage. The time constraints of other classes ended up not giving
us enough time to implement a full algorithm, so this ended up being a good compromise. 

The first change we made was to reclaim the memory of temporary tac variables when they are used, this differs a bit from
DCE as the scope of the temporaries are decided from the AST and not from a liveness analysis, but what
this allowed for was to store the first 5 temporaries in registers, and since temporaries are reassigned,
registers would also be reused. The only downside of this is that the effective register allocation strategy
is first-come-first-serve rather than using any priority heuristics, however it ended up working well enough
in the end. The second chane, or rather slate of changes, was to minimize the need for temporaries elsewhere.
The two main places this occured was to pre-allocate space calls and dispatch arguments, which are all stored
on the stack, to allow for constant alignment and the ability to free the temporaries needed for one argument
evaluation. The other place was in conditions, where new TAC instructions were added that implicitly store
the comparison results in "COND" or the condition flags, which assume that the next instruction will be a conditional
move or jump to use this flag. The result is that you do not need to use a SET instruction, read the result of that
instruction, and the compare it to zero, but can rather use a single conditional instruction if possible. With these,
a decent amount of the operators in functions are now stored in registers, and while it could have been better
and more complete, it was a very worthwhile compromise between time and performance.

The only caveat to this is that these changes, specifically regarding method calling and the "COND" implicit
instructions seem to have broken two and one test case respectively. I (Zachary) wish I had more time to fix these,
but I just have not been able to find the errors they are causing, as they do not break anything locally. I had to
decide between using a previous commit without these changes that don't break anything, or to include about a month
or two of work in our final submission with a few extra points off, and in the end I decided to go with the latter,
as I think it better demonstrates the work we did.

Once some somewhat optimized assembly code has been generated, we then apply some peephole optimizations to it.
These optimizations consist of algebraic simplifications, changing some instructions to faster ones (e.g. 
mult -> add), and removing unnecessary move instructions. We identify what instructions to remove by looking at
them in isolation to see if they are wholly unnecessary, like adding 0 or multiplying by 1, and in pairs. 