# All You Need is Lambda

## What's a function?

A function is a relation between two sets, a set of inputs and a set of outputs.

Functional programming language is a a computer programming paradigm that relies on definitionsand application of functions as the basis for programs. The essence fo functional programming is that programs are a combination of expressions to be evaluated.

Functional programming languages are all based on the lambda calculus and rely on expressions, but they are not all equally pure. 
* Pure FP is just the lambda calculus.
* Impure FP adds ambient effects, mutation and imperative step-wise encoding of instructions.

Being pure and functional leads to Haskell having a high degree of abstraction and composability. 

Haskell features **minimalist syntax, abstract and pure computations**.

## Lotsa lambdas

The lambda calculus has three basic components:
* **Expressions**, a variable name, an abstration, a function application or some combinations. 
* **Variables**, gives a name to a input.
* **Abstractions**.

Functions consist of two parts:
* **Head**, a lambda followed by a parameters.
* **Body**.

## Beta reduction

When you apply the function, you substitute the input expression for all instances of bound variables within the abstraction, a process known as **beta-reduction**. The reduction is all we can do in the lambda calculus. 

When there are no more heads or labmdas, you are done. A computation consists of 
* An initial labmda expression.
* A finite sequence of lambda terms , each deduced from the preceding term by beta-reduction.

An example of apply *λx.xy* to *z*.
1. *(λx.xy)z*. Apply the lambda to the argument. *x* is therefore bound to *z*. 
2. *(λ[x:=z].xy)*. Replace all *x* with *z*. 
3. *(λ[x:=z].zy)*. Change expression *xy* to *zy*.
4. *zy*. Drop the head of the labmda. 

## Bound and free variables

For example, in lambda *λx.xy*. *y* isn't appear in the head of the function, hence it's a free variable. It remains unreducible. 

We can bind multiple variables in a function, like *λxy.xy*. We eliminate one head at a time, starting from the leftmost head. 

## Alpha equivalence

For example, *λx.x*, *λd.d*, *λz.z* has the same meaning. 

## Multiple arguments

Each labmda accepts one argument. 

e.g. *λxy.x*, is a convenient short-hand for *λx(λy.x)*. And we are going to apply it to arguments:
1. *λx(λy.x) 1 2*
2. *(λy.1) 2*
3. *λ2.1*
4. *1*

## Evaluation is simplificaiton

**Beta Normal Form** is when you cannot beta reduce the terms any further, which corresponds to a *fully evaluated* expression. We call it *fully executed* program in programming.

## Combinators

A **combinator* is a lambda term with no free variabls, which serves only to combine the argument it is given.

Following are combinators:
* *λx.x*
* *λxy.x*
* *λxyz.xz(yz)*

And the following are not combinators:
* *λy.x*
* *λx.xz*

The fun thing about combinators is that *they can take one or more functions as their inputs and return another function as an output*. Combinators provide for higher-order functions as well as certain types of recursive functions in programming. 

## Divergence

Not all reducible lambda terms will reduce neatly to a beta normal form. This isn't because they're already fully reduced, but rather because they **diverge**. **Divergence** means that the reduction process never terminates or ends. For example:
1. *(λx.xx)(λx.xx)*
2. *([x:=(λx.xx)]xx)*
3. *(λx.xx)(λx.xx)*

## Summary

* FP is based on expressions that consist of variables or constant values alone, expressions combined with other expressions, and functions. 
* Expressions are evaluated, or reduced, to a result.
* Functions have a head and a body, and are applied to arguments.
* Variables amy be bound in the function declaration, and every time a bound variable shows up in a function, it has the same value.
* All functions take one argument and return one result.
* Functions are a mapping of a unique set of inputs to a unique set of outputs. Given the same input, they always return the same result.

## Definition

* The **lambda** in a lambda calculus is the greek letter *λ* used to introduce, or abstract, arguments for binding in an expression.
* A lambda **abstraction** is an anoymous function or labmda term. 
* **Application** is how one evaluates or reduces lambdas, binding the argument to whatever the lambda was applied to.
* **Labmda calculus** is a formal system for expressing programs in terms of abstraction and application. 
* **Normal order** is a common evaluation strategy in lambda calculi, meaning evaluating the leftmost outermost lambdas first, evaluating terms nested whihin after you've run out of arguments to apply.
