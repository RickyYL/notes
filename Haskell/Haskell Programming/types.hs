module Types where

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini
                  | Mazda
                  | Tata
                  deriving (Eq, Show)

data Airline = PapuAir
             | AirChina
             | ANA
             deriving (Eq, Show)

data Vehicle = Car Manufacturer Price 
             | Plane Airline
             deriving (Eq, Show)

mycar = Car Mini (Price 14000)
urcar = Car Mazda (Price 20000)
clowncar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar (Plane _) = False

isPlane :: Vehicle -> Bool
isPlane x = not $ isCar x

areCars :: [Vehicle] -> [Bool]
areCars x = map isCar x

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu