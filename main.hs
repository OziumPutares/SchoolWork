import Control.Monad.RWS.Class (MonadState (put))
import Data.ByteString (StrictByteString)
import Data.Char (toLower)
import Distribution.Compat.Lens (use)
import GHC.IO.FD (openFile)
import GHC.OldList (find)
import System.Posix (fileExist)
import Text.Printf (errorBadArgument)
import Text.Read (Lexeme (String))
import Text.XHtml (content, password)

collapseListExceptLastOp :: [String] -> (String -> String -> String) -> [String]
collapseListExceptLastOp [] op = error "List can't be empty"
collapseListExceptLastOp [y] op = [y]
collapseListExceptLastOp [x, y] op = [x, y]
collapseListExceptLastOp (y : ys@(z : _)) op =
  let collapsedList = collapseListExceptLastOp ys op
   in op
        y
        (head collapsedList)
        : tail collapsedList

collapseListExceptLast :: [String] -> [String]
collapseListExceptLast x = collapseListExceptLastOp x (++)

charParser :: (Char -> Bool) -> String -> [String] -> [String]
charParser x (y : ys) []
  | x y = charParser x ys []
  | otherwise = charParser x ys [[y]]
charParser x (y : ys) list
  | x y = charParser x ys (list ++ [""])
  | otherwise = charParser x ys (init list ++ [last list ++ [y]])
charParser x "" list = list

makeCharParser :: Char -> (String -> [String])
makeCharParser x y = charParser (x ==) y []

loadFile :: String -> IO String
loadFile filename = do
  fileExists <- fileExist filename
  if fileExists
    then do
      readFile filename
    else
      error "No such file"

stripEmptyLists :: [[a]] -> [[a]]
stripEmptyLists [] = []
stripEmptyLists (x : xs)
  | null x = stripEmptyLists xs
  | otherwise = x : stripEmptyLists xs

getStoredUserData :: String -> IO [[String]]
getStoredUserData filename = do
  contents <- loadFile filename
  let lineList = stripEmptyLists (lines contents)

  let users = map (makeCharParser ':') lineList
  let usernameToPassword = map (\users -> collapseListExceptLastOp users (\y x -> y ++ (':' : x))) users
  return usernameToPassword

handleLogin :: IO ()
handleLogin =
  do
    usernameToPassword <- getStoredUserData "./data.txt"
    putStrLn "Enter username"
    username <- getLine

    putStrLn "Enter password"
    password <- getLine
    if [map toLower username, password] `elem` usernameToPassword
      then
        putStrLn "Successfully signed in"
      else do
        putStrLn "Unsuccessful login"
        print usernameToPassword

getUsername :: IO String
getUsername = do
  userData <- getStoredUserData "./data.txt"
  let usernames = map head userData
  putStrLn "Enter username"
  username <- getLine
  if map toLower username `elem` usernames
    then do
      putStrLn "User already exists try again"
      getUsername
    else
      return (map toLower username) -- Return the valid username

getPassword :: IO String
getPassword = do
  putStrLn "Enter password"
  password <- getLine
  if ':' `elem` password
    then do
      putStrLn "Password cannot contain ':'. Please try again."
      getPassword -- Recursively ask for input again
    else
      return password -- Return the valid password

handleSignUp :: IO ()
handleSignUp =
  do
    username <- getUsername
    password <- getPassword
    appendFile "./data.txt" ("\n" ++ username ++ ":" ++ password)
    putStrLn (username ++ ":" ++ password)

main :: IO ()
main = do
  putStrLn "I for sign in, U for sign up"
  response <- getLine
  if response == "U"
    -- Handle signing up
    then handleSignUp
    else
      -- Handle logging in
      if response == "I"
        then do
          putStrLn "Signing in"
          handleLogin
        else putStrLn "Invalid option quitting"
