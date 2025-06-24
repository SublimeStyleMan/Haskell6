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
person1 = Individual { clientId = 1001
                     , person = Person {firstName = "John"
                                       , lastName = "Doe"
                                       , gender = Male}}   

person2 :: Client Int 
person2 = Individual { clientId = 1005
                     , person = Person {firstName = "Jiggy"
                                       , lastName = "Stardust"
                                       , gender = Female}}   

gov1 :: Client Int 
gov1 = Gov { clientId = 1002
           , clientName = "United States"}
gov2 :: Client Int 
gov2 = Gov { clientId = 1006
           , clientName = "Germany"}           

company1 :: Client Int
company1 = Company {  clientId = 1003
                    , clientName = "Wacky Corp"
                    , person = Person { firstName = "Jason"
                                      , lastName = "Vortex"
                                      , gender=Male}
                    , duty="Slasher"}

company2 :: Client Int
company2 = Company {  clientId = 1004
                    , clientName = "Tower Defense Inc"
                    , person = Person { firstName = "Freddy"
                                      , lastName = "FreeMan"
                                      , gender=Male}
                    , duty="Slasher"}
                    

person3 :: Client Int 
person3 = Individual { clientId = 1007
                     , person = Person {firstName = "ZuzuMu"
                                       , lastName = "Stardust"
                                       , gender = Female}}   

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

clients :: [Client Int]
clients = [
    person1
    , person2
    , person3
    , company1
    , company2
    , gov1
    , gov2 ]


-- find the amout of each gender for your clients 
data Genders = MF { male :: Int 
                  , female :: Int}
                  deriving (Show, Eq, Ord)

getGender :: Client a -> Maybe Gender 
getGender (Gov { }) = Nothing 
getGender (Company { person = Person { gender }}) = Just gender 
getGender (Individual {person = Person {gender}}) = Just gender 

updateMFLedger :: Genders -> Client a -> Genders
updateMFLedger l@(MF {male = m, female = f}) client = case (getGender client) of 
                                                       Just Male -> MF { male = m +1, female =f}
                                                       Just Female -> MF {male = m, female = f + 1}
                                                       Nothing -> l
emptyledger :: Genders 
emptyledger = MF {male = 0, female = 0}     

getGenderLedger :: [Client a] -> Genders
getGenderLedger cl = foldl (updateMFLedger) emptyledger cl

theLedger :: Genders 
theLedger = getGenderLedger clients

--filterNot :: (a -> b) ->[a]
notFun = (\fun x -> not $ fun x)
filterNot fun = filter $ notFun fun

myFold :: (a -> a -> a) -> a -> [a] -> a 
myFold f i [] = i 
myFold f i (x:xs) = f (myFold f i xs) x

-- minimumClient 
-- Uses name field puns
getName :: Client a -> String 
getName cl = case cl of 
                Gov { clientName} -> clientName 
                Company {clientName } -> clientName 
                Individual { person = Person { lastName, firstName}} -> firstName ++ " " ++ lastName

-- There is a problem here in that two names could be equal in which case it would take the 
-- other one
biggerName :: Client a -> Client a-> Client a
biggerName f s = let fClient = getName f
                     sClient = getName s 
                 in if (length fClient) > (length sClient)
                        then s
                    else f
                          
-- This is an interesting case because if a list has somthing in it,  
-- It will always have a value and Nothin isn't really a base case
minimumClient :: [Client a] -> Maybe  (Client a)
minimumClient [] = Nothing
minimumClient [x] = Just x 
minimumClient (x:xs) = biggerName <$> (Just x) <*> minimumClient xs


minimumBy :: Ord a => (b ->a) -> [b] -> a
minimumBy g xs = foldl1 (min) $ map g xs

                             
                           

        