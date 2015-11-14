# Basic Datatypes

Types are a powerful means of classifying, organzing, and delimiting data to only the forms we want to process in our programs. Types provide the means to build programs more quickly and also allow for greater ease of maintenance.

## Anatomy of a data declaration

The **type constructor** is the name of the type and is capitalized. When you are reading or writing type signatures, the type names or type constructors are what you use.

The data constructor is the values that inhabit the type they are defined in. They are the values that show up in your code.

```
data Bool = False  |  True
--   [1]     [2]  [3] [4]
```

* **[1]** - Type constructor for datatype Bool. This is the name of the type and shows up in the signature.
* **[2]** - Data constructor for the value False.
* **[3]** - Pipe | indicates logical disjunction, "or". 
* **[4]** - Data constructor for the value True.

The whole thing is called a **data declaration**.

## Numeric types

* Integral numbers: whle numbers, positive and negative.
	* Int
	* Integer
* Fractional: Not integers. Fractional numbers include the following fure types:
	* Float
	* Double
	* Rational
	* Scientific

These numberic datatypes all have instances of a typeclass `Num`. Typeclases are a way of adding functionality to types that is reusable across all the types that have instances of that typeclass.

## Tuples

## Lists