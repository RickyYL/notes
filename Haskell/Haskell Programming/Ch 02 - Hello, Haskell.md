# Hello, Haskell!

## Understanding expressions

Everything in Haskell is an expression or declaration. 
* Expressions evaluate to a result. Expressions are the building blocks of our programs, and programs themselves are one big expression made of smaller expressions.
* Declarations are top-level bindings which lets us give names to expressions which we can refer to them multiple times without copying and pasting the expressions.

We say that expressions are in **Normal Form** when they have reached an irreducible form. Reducible expressions are also called **redexes**. 

## Functions

Functions are a specific type of expression. Functions in Haskell are related to functions in mathematics, which is to say they map an inpuut or a set of inputs to an output. A function is an expression that is applied to an argument and always retuns a result. 

### Capitalization matters
* Names of modules and names of types, start with a capital letter. 
* Function names start with lowercase letters. 
* Variables are lowercase.

### Conventions for variables

* Type variables generally start at *a* and go from there: *a,b,c*, and so forth. 
* Functions can be used as arguments and in that case are tpically labeled with variables starting at *f*, followed by *g* and so on. 
* Arguments to functions are most often given names starting at *x*. 
* For a list of things named *x*, it's often called *xs*. *(x:xs)* means a list starts with *x*, and the rest of the list are *xs*.

## Declaring values

To declare the same values in a file, we write the following:

```
module Learn where

x = 10 * 5 + y
myResult = x * 5
y = 10
```

Indentations of Haskell code is significant and can change the meaning of the code. Also, trailing whitespace is considered bad style. 

## Let vs. Where

* `let .. in ..` is an expression.
* `where` is bound to a surrounding syntactic construct. 