module CountLines where

main :: IO ()
main = interact (\s -> show (length (lines s)) ++ "\n")
