# CS 32 Exam 02

## Topics

* command line compile
* linking
* Makefile
* Hask tables
 * definitions
 * collision
 * clustering and solution

## Homework Smmary

* H11 - PS9 (11.1, 11.2) DS4 (2.5) Friend Functions, Operator Overloading
* H12 - PS9 (11.3) Arrays and Classes
* H13 - PS9 (11.4) THE BIG-3 (Classes and Dynamic Arrays)
* H14 - PS9 (16.1, 16.2) Exception Handling
* H15 - DS4 (Ch 3) Container Classes


* H16 - READER (p. 55-70) Memory Management
* H17 - READER (p. 83-116) OS, Unix and Shells
* H18 - READER (p. 117-146) Processes

## Notes

### Friend or member function
* Use a member function if the task being performed by the function involves only one object.
* Use a nonmember function if the task being performed involoves more than one object. 

### Rules on Overloading Operators
* At least one argument of the resulting overloaded operator must be of a class type.
* An overloeaded operator can be but does not have to be a friend of a class. 
* You cannot create new operator.
* You cannot change the number of arguments that an operator takes.
* You cannot change the precedence of an operator.
* The following operators cannot be overloaded: `(.)`, `(::)`, `(.*)` and `(?:)`.

#### Overloading `<<` and `>>`
* `istream& operator>>(istream& is, Class& op);`
* `ostream& operator<<(ostream& os, const Class& op);`

#### Overloading `++` and `--`
* Prefix - `Class& operator++()` 
* Postfix - `Class operator++(int)`

### The Big-3

The **copy constructor**, the **assignemt operator**, and the **destructor** are called the big-3 because experts say that if you need to define any of them, then you need to define all three.

A **copy constructor** is a constructor that has one call-by-reference parameter that is of the same type as the calss. 

