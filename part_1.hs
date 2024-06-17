
import qualified Data.Map as Map

data Person = Person 
  { pFirstName :: String 
  , pLastName :: String
  , pAge :: Maybe Int
  }
  deriving (Show)

data User = User
  { uLogin :: String
  , uPerson :: Maybe Person
  }
  deriving (Show)
  
type Users = Map.Map String User


getPersonName :: Person -> String
getPersonName (Person fn ln _) = fn ++ " " ++ ln

getPersonAge :: Person -> Maybe Int
getPersonAge (Person _ _ mbAge) = mbAge

getPerson :: User -> Maybe Person
getPerson (User _ mbPerson) = mbPerson

getUser :: String -> Users -> Maybe User
getUser login users = Map.lookup login users

  -- Data base -- 
    -- Persons -- 
bobPerson = Person "Bob" "Fisher" Nothing
alicePerson = Person "Alice" "Munro" (Just 19)
    -- Users is a dictionary of key value pairs :: Users <=> Map.Map String User
users :: Users
users = Map.fromList
  [ ("bob",       User "bob" (Just bobPerson))
  , ("alice",     User "alice" (Just alicePerson))
  , ("incognito", User "incognito" Nothing)
  ]
  

describePerson :: String -> String
describePerson login  = 
  case getUser login users of
    Nothing   -> error ("There is no such login: " ++ login)
    Just user ->
  
      case getPerson user of
        Nothing     -> error ("There is no person associated with this login: " ++ login)
        Just person -> 
  
          case pAge person of
            Nothing  -> (pFirstName person ++ " " ++ pLastName person)
            Just age -> (pFirstName person ++ " " ++ pLastName person ++ " " ++ show age)



main :: IO ()
main = print (describePerson "alice") 
