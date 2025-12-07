module Main where
import Library

main :: IO ()
main = do
    putStrLn "Library Management System"
    let library = []
    let users = []
    mainLoop library users
