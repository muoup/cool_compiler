Our PA2 implementation was designed to have four main stages...

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
Our good test case checks that we inherit IO methods correctly, which had caused us problems. It also makes sure
our while loops don't break when given an expression.

