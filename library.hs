module Library where
import Prelude hiding (map, filter)
import Data.List (find)

main :: IO ()
main = do
    putStrLn "Library Management System"
    let library = [
            Book "1" "1984" "George Orwell" True,
            Book "2" "To Kill a Mockingbird" "Harper Lee" True,
            Book "3" "The Great Gatsby" "F. Scott Fitzgerald" True,
            Book "4" "Pride and Prejudice" "Jane Austen" True,
            Book "5" "The Catcher in the Rye" "J.D. Salinger" False
            ]
    let users = [
            User "101" "Alice Johnson",
            User "102" "Bob Smith"
            ]
    mainLoop library users

mainLoop :: Library -> [User] -> IO ()
mainLoop library users = do
    putStrLn "1. Add Book"
    putStrLn "2. Remove Book"
    putStrLn "3. List Books"
    putStrLn "4. Add User"
    putStrLn "5. Remove User"
    putStrLn "6. List Users"
    putStrLn "7. Borrow Book"
    putStrLn "8. Return Book"
    putStrLn "9. Exit"
    putStr "Choose an option: "
    option <- getLine
    case option of
        "1" -> do
            putStr "Enter book ID: "
            bookID <- getLine
            putStr "Enter book title: "
            bookTitle <- getLine
            putStr "Enter book author: "
            bookAuthor <- getLine
            let book = Book bookID bookTitle bookAuthor True
            mainLoop (addBook book library) users
        "2" -> do
            putStrLn "Enter book ID to remove:"
            id <- getLine
            mainLoop (removeBook id library) users
        "3" -> do
            putStrLn "\nBooks in Library:"
            mapM_ (\book -> putStrLn $ uniqueID book ++ ": " ++ title book ++ " by " ++ author book ++ 
                   " - " ++ (if status book then "Available" else "Borrowed")) library
            putStrLn ""
            mainLoop library users
        "4" -> do
            putStr "Enter user ID: "
            userID <- getLine
            putStr "Enter user name: "
            userName <- getLine
            let user = User userID userName
            mainLoop library (addUser user users)
        "5" -> do
            putStrLn "Enter user ID to remove:"
            id <- getLine
            mainLoop library (removeUser id users)
        "6" -> do
            putStrLn "\nUsers in System:"
            mapM_ print (listUsers users)
            putStrLn ""
            mainLoop library users
        "7" -> do
            putStr "Enter book ID to borrow: "
            bookID <- getLine
            putStr "Enter user ID: "
            userID <- getLine
            case lookupBookByID bookID library of
                Just book | status book -> do
                    let (updatedLibrary, updatedUsers) = borrowBook bookID userID library users
                    putStrLn "Book borrowed successfully!"
                    mainLoop updatedLibrary updatedUsers
                          | otherwise -> do
                    putStrLn "Book is already borrowed!"
                    mainLoop library users
                Nothing -> do
                    putStrLn "Book not found!"
                    mainLoop library users
        "8" -> do
            putStr "Enter book ID to return: "
            bookID <- getLine
            putStr "Enter user ID: "
            userID <- getLine
            case lookupBookByID bookID library of
                Just book | not (status book) -> do
                    let (updatedLibrary, updatedUsers) = returnBook bookID userID library users
                    putStrLn "Book returned successfully!"
                    mainLoop updatedLibrary updatedUsers
                          | otherwise -> do
                    putStrLn "Book was not borrowed!"
                    mainLoop library users
                Nothing -> do
                    putStrLn "Book not found!"
                    mainLoop library users
        "9" -> putStrLn "Exiting..."
        _   -> do
            putStrLn "Invalid option. Please try again."
            mainLoop library users

data Book = Book {
    uniqueID :: String,
    title :: String,
    author :: String,
    status :: Bool
} deriving (Show, Eq)

data User = User {
    userID :: String,
    name :: String
} deriving (Show, Eq)

type Library = [Book]

parseBookDetails :: String -> Book
parseBookDetails input = 
    let parts = words input
    in Book (parts !! 0) (parts !! 1) (parts !! 2) True

parseUserDetails :: String -> User
parseUserDetails input = 
    let parts = words input
    in User (parts !! 0) (unwords (drop 1 parts))

addBook :: Book -> Library -> Library
addBook book library = book : library


removeBook :: String -> Library -> Library
removeBook id = filter (\book -> uniqueID book /= id)


addUser :: User -> [User] -> [User]
addUser user users = user : users


removeUser :: String -> [User] -> [User]
removeUser id = filter (\user -> userID user /= id)


borrowBook :: String -> String -> Library -> [User] -> (Library, [User])
borrowBook bookID userID library users =
    case lookupBookByID bookID library of
        Just book | status book -> (updateBookStatus bookID library False, users)
                   | otherwise -> (library, users)
        Nothing -> (library, users)


returnBook :: String -> String -> Library -> [User] -> (Library, [User])
returnBook bookID userID library users =
    case lookupBookByID bookID library of
        Just book | not (status book) -> (updateBookStatus bookID library True, users)
                   | otherwise -> (library, users)
        Nothing -> (library, users)

lookupBookByID :: String -> Library -> Maybe Book
lookupBookByID id = find (\book -> uniqueID book == id)


updateBookStatus :: String -> Library -> Bool -> Library
updateBookStatus id library newStatus =
    map (\book -> if uniqueID book == id then book { status = newStatus } else book) library


listBooks :: Library -> [String]
listBooks = map title


listUsers :: [User] -> [String]
listUsers = map name


map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

lookupBookByTitle :: String -> Library -> Maybe Book
lookupBookByTitle searchTitle = find (\book -> title book == searchTitle)

getBookStatus :: String -> Library -> Maybe Bool
getBookStatus title library = fmap status (lookupBookByTitle title library)