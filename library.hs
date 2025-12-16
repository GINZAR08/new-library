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
            User "101" "Alice Johnson" "user123",
            User "102" "Bob Smith" "user456"
            ]
    loginScreen library users

loginScreen :: Library -> [User] -> IO ()
loginScreen library users = do
    putStrLn "\n=== LOGIN ==="
    putStrLn "1. Login as Admin"
    putStrLn "2. Login as User"
    putStrLn "3. Create New Account"
    putStrLn "4. Exit"
    putStr "Choose an option: "
    option <- getLine
    case option of
        "1" -> do
            putStr "Enter admin password: "
            password <- getLine
            if password == "admin123"
                then adminMenu library users
                else do
                    putStrLn "Incorrect password!"
                    loginScreen library users
        "2" -> do
            putStr "Enter user ID: "
            uid <- getLine
            putStr "Enter password: "
            password <- getLine
            case find (\u -> userID u == uid && userPassword u == password) users of
                Just user -> userMenu library users uid
                Nothing -> do
                    putStrLn "Invalid credentials!"
                    loginScreen library users
        "3" -> do
            putStr "Enter new user ID: "
            uid <- getLine
            case find (\u -> userID u == uid) users of
                Just _ -> do
                    putStrLn "User ID already exists!"
                    loginScreen library users
                Nothing -> do
                    putStr "Enter your name: "
                    userName <- getLine
                    putStr "Create password: "
                    password <- getLine
                    let newUser = User uid userName password
                    putStrLn "Account created successfully!"
                    loginScreen library (newUser : users)
        "4" -> putStrLn "Exiting..."
        _ -> do
            putStrLn "Invalid option!"
            loginScreen library users

adminMenu :: Library -> [User] -> IO ()
adminMenu library users = do
    putStrLn "\n=== ADMIN MENU ==="
    putStrLn "1. Add Book"
    putStrLn "2. Remove Book"
    putStrLn "3. List Books"
    putStrLn "4. Add User"
    putStrLn "5. Remove User"
    putStrLn "6. List Users"
    putStrLn "7. Logout"
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
            putStrLn "Book added successfully!"
            adminMenu (addBook book library) users
        "2" -> do
            putStr "Enter book ID to remove: "
            id <- getLine
            putStrLn "Book removed successfully!"
            adminMenu (removeBook id library) users
        "3" -> do
            putStrLn "\nBooks in Library:"
            mapM_ (\book -> putStrLn $ uniqueID book ++ ": " ++ title book ++ " by " ++ author book ++ 
                   " - " ++ (if status book then "Available" else "Borrowed")) library
            putStrLn ""
            adminMenu library users
        "4" -> do
            putStr "Enter user ID: "
            userID <- getLine
            putStr "Enter user name: "
            userName <- getLine
            putStr "Enter user password: "
            userPassword <- getLine
            let user = User userID userName userPassword
            putStrLn "User added successfully!"
            adminMenu library (addUser user users)
        "5" -> do
            putStr "Enter user ID to remove: "
            id <- getLine
            putStrLn "User removed successfully!"
            adminMenu library (removeUser id users)
        "6" -> do
            putStrLn "\nUsers in System:"
            mapM_ (\user -> putStrLn $ userID user ++ ": " ++ name user) users
            putStrLn ""
            adminMenu library users
        "7" -> loginScreen library users
        _ -> do
            putStrLn "Invalid option. Please try again."
            adminMenu library users

userMenu :: Library -> [User] -> String -> IO ()
userMenu library users currentUserID = do
    putStrLn "\n=== USER MENU ==="
    putStrLn "1. List Books"
    putStrLn "2. Borrow Book"
    putStrLn "3. Return Book"
    putStrLn "4. Logout"
    putStr "Choose an option: "

    option <- getLine
    case option of
        "1" -> do
            putStrLn "\nAvailable Books:"
            mapM_ (\book -> putStrLn $ uniqueID book ++ ": " ++ title book ++ " by " ++ author book ++ 
                   " - " ++ (if status book then "Available" else "Borrowed")) library
            putStrLn ""
            userMenu library users currentUserID
        "2" -> do
            putStr "Enter book ID to borrow: "
            bookID <- getLine
            case lookupBookByID bookID library of
                Just book | status book -> do
                    let (updatedLibrary, updatedUsers) = borrowBook bookID currentUserID library users
                    putStrLn "Book borrowed successfully!"
                    userMenu updatedLibrary updatedUsers currentUserID
                          | otherwise -> do
                    putStrLn "Book is already borrowed!"
                    userMenu library users currentUserID
                Nothing -> do
                    putStrLn "Book not found!"
                    userMenu library users currentUserID
        "3" -> do
            putStr "Enter book ID to return: "
            bookID <- getLine
            case lookupBookByID bookID library of
                Just book | not (status book) -> do
                    let (updatedLibrary, updatedUsers) = returnBook bookID currentUserID library users
                    putStrLn "Book returned successfully!"
                    userMenu updatedLibrary updatedUsers currentUserID
                          | otherwise -> do
                    putStrLn "Book was not borrowed!"
                    userMenu library users currentUserID
                Nothing -> do
                    putStrLn "Book not found!"
                    userMenu library users currentUserID
        "4" -> loginScreen library users
        _ -> do
            putStrLn "Invalid option. Please try again."
            userMenu library users currentUserID

data Book = Book {
    uniqueID :: String,
    title :: String,
    author :: String,
    status :: Bool
} deriving (Show, Eq)

data User = User {
    userID :: String,
    name :: String,
    userPassword :: String
} deriving (Show, Eq)

type Library = [Book]

parseBookDetails :: String -> Book
parseBookDetails input = 
    let parts = words input
    in Book (parts !! 0) (parts !! 1) (parts !! 2) True

parseUserDetails :: String -> User
parseUserDetails input = 
    let parts = words input
    in User (parts !! 0) (unwords (drop 1 (init parts))) (last parts)

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