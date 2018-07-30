module Main where

main :: IO ()
main = do
  putStrLn "hello world"

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldFunc [] . words
-- solveRPN string =
--   case foldl foldFunc [] . words $ string of
--     (x:_) -> x
--     _ -> 0
--   where
    foldFunc (x:y:ys) "*" = (x * y) : ys
    foldFunc (x:y:ys) "+" = (x + y) : ys
    foldFunc (x:y:ys) "-" = (x - y) : ys
    foldFunc xs number = read number : xs
