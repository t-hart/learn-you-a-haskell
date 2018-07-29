import Data.List
import System.Directory
import System.Environment
import System.IO

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add), ("view", view), ("remove", remove)]

invalid :: IO ()
invalid = putStrLn "Invalid input"

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName ("\n" ++ todoItem)
add _ = invalid

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks =
        zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStr $ unlines numberedTasks
view _ = invalid

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  handle <- openFile fileName ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile fileName
  renameFile tempName fileName
remove _ = invalid

main = do
  (command:args) <- getArgs
  let (Just action) = lookup command dispatch
  action args
