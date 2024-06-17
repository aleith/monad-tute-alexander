
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
  
--                   key    value  
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
  

describePerson :: String -> Maybe String
describePerson login  = 
  case getUser login users of
    Nothing   -> error ("There is no such login: " ++ login)
    Just user ->
  
      case getPerson user of
        Nothing     -> error ("There is no person associated with this login: " ++ login)
        Just person -> 
  
          case pAge person of
            Nothing  -> Just (pFirstName person ++ " " ++ pLastName person)
            Just age -> Just (pFirstName person ++ " " ++ pLastName person ++ " " ++ show age)


describePerson1 :: String -> Maybe String
describePerson1 login = do
  user      <- getUser login users
  person    <- getPerson user

  case pAge person of
    Nothing  -> Just (pFirstName person ++ " " ++ pLastName person)
    Just age -> Just (pFirstName person ++ " " ++ pLastName person ++ " " ++ show age)



-- getUser :: String -> Users -> Maybe User        -- monadic function
-- getPerson :: User          -> Maybe Person      -- monadic function
-- getPersonAge :: Person     -> Maybe Int         -- monadic function
-- pAge :: Person             -> Maybe Int         -- accessor from the data type: Person 
  

main :: IO ()
main = print (describePerson1 "alice") 
