module TimeMachinesTypes where 

data Direction = Past | Present | Future

data TimeMachine a = TimeMachine 
                    { manufacturer :: String 
                     , model :: a 
                     , name :: String
                     , direction :: Direction}