Our PA2 implementation is designed in four main stages. First, the input is read from the .cl-ast file. There
is not a whole ton to be said about this stage as it is probably almost identical to how everyone else did it.
The one thing that stuck out during the implementation of this phase is just how well-equipped the functional
paradigm of OCaml is for parsing. It was mentioned in class but getting to experience it in a real implementation 
was a good way to realize just how well everything slots together.

The second phase is two closely related sub-procedures. For the most part the design of this regards the checks
for PA2C2, which are split between "g_verify_classes", what could be considered "contextless" checks, for instance
checking if a class inherits from Int, Bool, or String, and the "contextual" checks such as looking for duplicate
inherited attributes of "g_verify_inheritance", which require knowledge of the class hierarchy. A simple data structure 
containing a list of individual class data records with a superclass and subclasses, attributes, and methods made 
aggregating the full sum of data for each class where needed much simpler and cleaner, and also was vital in the 
implementation of the full PA2 as well. Much of this implementation did not have the more complicated algorithms
and pertained more to just checking for very specific non-expression based errors.

The third and fourth phase pertain to typechecking expressions required by PA2, and outputting the .cl-type file. As
is documented some in the code as well, due to the parallel nature of the development of these two phases, there is
some definite redundant overlap of features implemented between the two. For instance, the function to typecheck an
expression returns the type of the expression as may be needed by an internal recursive calls, however the outputting
stage has its own get_expr_type function to get the type of an expression as well. This is definitely an artifact of
time crunch and a lesson in planning for the future, however it was not all that bad given that the output's expression
typing could assume that the AST was well-formed and thus was much simpler to implement. 

As well this unintentionally may have made implementing the more complicated expressions a bit easier as well, such as
case, if, and dispatch, as these were implemented in the typechecker after the output was finished, meaning that they
were reasoned in a simpler environment first and the necessary helper functions needed were there already to be used
for the more challenging requirements of typechecking. Much of the implementation of dispatch borrows from what
was necessary for output, given collecting all of the methods that a class had access to was required for the implementation
map, so the procedure for inheritance tree traversal to get unique methods was pretty easily applied to dispatch after
output had been completed. The join algorithm is pretty simple albeit a bit inefficient, but worked very well in all of
its use cases. Essentially one class will create a set of all of its inherited classes from itself to Object, and
the other class traverses up its inheritance tree, using the work of e_ast_data from PA2C2, and finds the first class
it stumbles into in the set. Finally, the last main algorithm needed to be implemented was subtype_of, which was a good
example of the convenience of a functional language, as the routine was a simple recursive pattern match that pushed
the left-side type up its inheritance tree until it either reached the right-side, or Object in which case it can be said
that it is not a subtype, along with "upgrading" or perhaps more accurately specifying SELF_TYPE to the current class being parsed
when needed. Finding the right application of these methods was again a similar trial-and-error to that of PA2C2, however
with the algorithms and proper data structures set up, finishing up the typechecker was much more manageable.

Our four test cases attempt to test a wide breadth of the Cool language features. The first bad test case
is some self and static dispatch trickery, where we have a class B which has a method copy1. B inherits A,
which does not have that copy1 method, so calling A.copy2() should throw an error, even though it is called 
on an object of type B, which does. 

The second bad test case simply tests that you cannot assign subtypes to supertypes (ints to objects). This test 
case is simpler but deserves inclusion because it revealed a very silly error where we had flipped the order of
our subtype check, saving us many hours.

The final bad test case tests our comparisons, which had caused us problems.
Although the method int_or_object in reality always returns an int, since a and b are 
both ints, its declared return type of object should make comparisons between it and an int illegal. We also
threw in an isvoid comparison and an IO inheritance to make sure that didn't break our code.

Our good test case checks that we inherit IO methods correctly, which had caused us problems. It uses both out_ and in_string,
and then uses the result in an expression of another method.
It also makes sure our while loops don't break when given an expression as a predicate.

