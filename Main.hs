module Main where
import Library

main :: IO ()
main = do
    putStrLn "Library Management System"
    let library = []
    let users = []
    mainLoop library users

mainLoop :: [a0] -> [a1] -> IO ()
mainLoop library users = do
    putStrLn "Main loop started"
    return ()
