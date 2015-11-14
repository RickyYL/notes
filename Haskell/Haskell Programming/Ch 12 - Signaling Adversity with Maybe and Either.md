# Signaling Adversity with Maybe and Either

## How I learned to stop worrying and love Nothing

```
data Maybe a = Nothing | Just a
```

```
ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n
               then Just (n + 2)
               else Nothing
```

### Smart constructors for datatypes

```
type Name   = String
tyoe Age    = Integer

data Person = Person Name Age derving Show

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age | name /= "" && age > 0 = Just $ Person name age
                  | otherwise = Nothing
```

Here, `mkPerson` is what we called a smart constructor. It allows us to construct values of a type only when they meet certain criteria, so that we know we have a valid value, and return an explicit signal when we do not.

## Bleating Either

SO to express why we didn't get a successful result back from our `mkPerson` constructor. We've got the `Either` datatype which is defined as follows in the Prelude:

```
data Either a b = Left a | Right b
```

To tell the error and its type, we start by making a simple sum type to enumerate our failure modes.

```
data PersonInvalid = NameEmpty | AgeTooLow derving (Eq, Show)
```

Next, the `mkPerson` constructor is going to change into:

```
mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age | name /= "" && age > 0 = Right $ Person name age
                  | name == "" = Left NameEmpty
				  | otherwise  = Left AgeTooLow
```

We use `Left` as our invalid or error constructor for a couple of reasons. It is conventional to do so in Haskell. 

And we improve the constructor by allowing it returns a list of errors.

```
type validatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age > 0 of 
              True  -> Right age
              False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Age
nameOkay name = case name /= "" of 
                True  -> Right name
                False -> Left [NameEmpty]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOK) = Right (Person nameOK ageOK)
mkPerson' (Left badName) (Left badAge) = Left  (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge)  = Left badAge
```

What's really going to bake you noodle is taht we'll be able to replace `mkPerson` and `mkPerson'` with the following later in this Book:

```
mkPerson :: Name -> Age -> Vlidation [PersonInvalid] Person
mkPerson name age = liftA2 Person (nameOkay name) (ageOkay age)
```

## Kinds, a thousand stars in your types

Kinds are types one level up. They are used to describe the types of type constructors. In Haskell Report, 
* **Type constant** is used to refer to types that take no arguments and are alread types.
* **Type constructor** is used to refer to types which must have arguments applied to become a type.

The kind `*` represents a concrete type. There is nothing left awaiting application. 

### Lifted and unlifted types

Kind `*` is the kind of all statndard lifted types, while primitive types have the kind `#` and are unlifted. 

A lifted type, which includes any datatype you could define yourself, is any that can be inhabited by bottom. Lifted types are represendted by a pointer and include most of the datatypes we've seen and most that you're likely to encounter and use.

Unlifted types are native machine types, represented by a value on the stack rather than a pointer. 