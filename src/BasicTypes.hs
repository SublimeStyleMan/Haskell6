{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
module BasicTypes where 

data Gender = Male | Female
                deriving (Show, Eq, Ord)

data Person = Person { firstName :: String, lastName :: String , gender :: Gender }
                deriving (Show, Eq, Ord)

data Client i = Gov {clientId :: i, clientName :: String }
             | Company { clientId :: i, clientName :: String, person :: Person, duty :: String }
             | Individual { clientId :: i, person :: Person }
             deriving (Show, Eq, Ord)


person1 :: Client Int 
person1 = Individual { clientId = 1001, person = Person {firstName = "John", lastName = "Doe", gender = Male}}   

gov1 :: Client Int 
gov1 = Gov {clientId = 1002, clientName = "United States"}

company1 :: Client Int
company1 = Company {  clientId = 1003
                    , clientName = "Wacky Corp"
                    , person = Person { firstName = "Jason"
                                      , lastName = "Vortex"
                                      , gender=Male}
                    , duty="Slasher"}
                    

clientNameFun :: Client a -> String 
clientNameFun (Gov { clientName = cn } ) = cn
clientNameFun (Company { person = Person { lastName = ln, firstName = fn}}) = fn ++ " " ++ ln
clientNameFun ( Individual {person = Person {lastName = ln, firstName = fn}}) = fn ++ " " ++ ln

responsibiliy :: Client a -> String 
responsibiliy (Gov {}) = "Government"
responsibiliy (Company { duty = dt}) = dt 
responsibiliy (Individual {}) = "Individual"

--example with view patterns 
-- it makes so that you can just run functions on different data structures and match within one function
specialEmployee :: Client a -> Bool 
specialEmployee (clientNameFun -> "John Doe" ) = True
specialEmployee (responsibiliy -> "Boss") = True 
specialEmployee _ = False

-- example for name pus. This makes it somewhat easier to get records
greet :: Client a -> String 
greet Gov {clientName} = "Welcome, " ++ clientName 
greet Company { person = Person {firstName, lastName}} = "Hi " ++ firstName ++ " " ++ lastName
greet Individual { person = Person {firstName, lastName}} = "You, " ++ firstName ++ " " ++ lastName ++ " are and individual. "



