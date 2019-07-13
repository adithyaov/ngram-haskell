module Test.Internal where

foldT :: [Bool] -> IO ()
foldT [] = return ()
foldT (x:xs) = if x then foldT xs else error "Test failure"


