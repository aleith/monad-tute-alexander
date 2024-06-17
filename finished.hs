
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

getPersonAge :: Person -> Either String Int
getPersonAge (Person _ _ mbAge) = case mbAge of
  Nothing  -> Left "Age is not specified"
  Just age -> Right age

getPerson :: User -> Either String Person
getPerson (User _ mbPerson) = case mbPerson of
  Nothing     -> Left "No person is specified"
  Just person -> Right person

getUser :: String -> Users -> Either String User
getUser login users = case Map.lookup login users of
  Nothing     -> Left ("User not found: " ++ login)
  Just user -> Right user

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
  
type Error = String

describePerson2 :: String -> Either Error String
describePerson2 login  = 
  case getUser login users of
    Left err1  ->  Left err1
    Right user ->
  
      case getPerson user of
        Left err2    -> Left err2
        Right person -> 
  
          case pAge person of
            Nothing  -> Right (pFirstName person ++ " " ++ pLastName person)
            Just age -> Right (pFirstName person ++ " " ++ pLastName person ++ " " ++ show age)


-- describePerson1 :: String -> Maybe String
-- describePerson1 login = do
--   user      <- getUser login users
--   person    <- getPerson user

--   case pAge person of
--     Nothing  -> Just (pFirstName person ++ " " ++ pLastName person)
--     Just age -> Just (pFirstName person ++ " " ++ pLastName person ++ " " ++ show age)



-- getUser :: String -> Users -> Either Error User        -- monadic function
-- getPerson :: User          -> Either Error Person      -- monadic function
-- getPersonAge :: Person     -> Either Error Int         -- monadic function
-- pAge :: Person             -> Maybe Int         -- accessor from the data type: Person 
  

main :: IO ()
main = do

  let (eDescr :: Either Error String) = describePerson2 "alice" -- describePerson is Either

  case eDescr of
    Right descr -> print descr 
    Left err    -> print err
  print (describePerson2 "bob") 
  
--  line <- getLine
  
