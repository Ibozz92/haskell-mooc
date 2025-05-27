module Set11a where

import Control.Monad
import Data.List
import System.IO

import Mooc.Todo

-- Lecture 11:
--   * The IO type
--   * do-notation
--
-- Useful functions / operations:
--   * putStrLn
--   * getLine
--   * readLn
--   * replicateM
--   * readFile
--   * lines
--
-- Do not add any new imports! E.g. Data.IORef is forbidden.

------------------------------------------------------------------------------
-- Ex 1: define an IO operation hello that prints two lines. The
-- first line should be HELLO and the second one WORLD

hello :: IO ()
hello = putStrLn "HELLO\nWORLD"

------------------------------------------------------------------------------
-- Ex 2: define the IO operation greet that takes a name as an
-- argument and prints a line "HELLO name".

greet :: String -> IO ()
greet name = do 
    putStrLn ("HELLO "++name)

------------------------------------------------------------------------------
-- Ex 3: define the IO operation greet2 that reads a name from the
-- keyboard and then greets that name like the in the previous
-- exercise.
--
-- Try to use the greet operation in your solution.

greet2 :: IO ()
greet2 = do
    a <- getLine
    greet a

------------------------------------------------------------------------------
-- Ex 4: define the IO operation readWords n which reads n lines from
-- the user and produces them as a list, in alphabetical order.
--
-- Example in GHCi:
--   Set11> readWords 3
--   bob
--   alice
--   carl
--   ["alice","bob","carl"]

readWords :: Int -> IO [String]
readWords a = do
    lis <- hReadWords a
    return (sort lis)

hReadWords :: Int -> IO [String]
hReadWords 0 = return []
hReadWords n = do
    a <- getLine
    b <- readWords (n-1)
    return (a : b)

------------------------------------------------------------------------------
-- Ex 5: define the IO operation readUntil f, which reads lines from
-- the user and returns them as a list. Reading is stopped when f
-- returns True for a line. (The value for which f returns True is not
-- returned.)
--
-- Example in GHCi:
--   *Set11> readUntil (=="STOP")
--   bananas
--   garlic
--   pakchoi
--   STOP
--   ["bananas","garlic","pakchoi"]

readUntil :: (String -> Bool) -> IO [String]
readUntil f = do
    a <- getLine
    if f a then do
        return []
    else do
        b <- readUntil f
        return (a : b)

------------------------------------------------------------------------------
-- Ex 6: given n, print the numbers from n to 0, one per line

countdownPrint :: Int -> IO ()
countdownPrint n = mapM_ print [n,(n-1)..0]

------------------------------------------------------------------------------
-- Ex 7: isums n should read n numbers from the user (one per line) and
--   1) after each number, print the running sum up to that number
--   2) finally, produce the sum of all numbers
--
-- Example:
--   1. run `isums 3`
--   2. user enters '3', should print '3'
--   3. user enters '5', should print '8' (3+5)
--   4. user enters '1', should print '9' (3+5+1)
--   5. produces 9

isums :: Int -> IO Int
isums = hisums 0

hisums :: Int -> Int -> IO Int
hisums n 0 = return n
hisums currentnum remaining = do
    newnum <- readLn
    let result = currentnum + newnum
    print result
    hisums result (remaining-1)

------------------------------------------------------------------------------
-- Ex 8: when is a useful function, but its first argument has type
-- Bool. Write a function that behaves similarly but the first
-- argument has type IO Bool.

whenM :: IO Bool -> IO () -> IO ()
whenM cond op = do
    b <- cond
    when b op

------------------------------------------------------------------------------
-- Ex 9: implement the while loop. while condition operation should
-- run operation as long as condition returns True.
--
-- Examples:
--   -- prints nothing
--   while (return False) (putStrLn "IMPOSSIBLE")
--
--   -- prints YAY! as long as the user keeps answering Y
--   while ask (putStrLn "YAY!")

-- used in an example
ask :: IO Bool
ask = do putStrLn "Y/N?"
         line <- getLine
         return $ line == "Y"

while :: IO Bool -> IO () -> IO ()
while cond op = do
    b <- cond
    if b then do 
        op
        while cond op
    else do return ()

------------------------------------------------------------------------------
-- Ex 10: given a string and an IO operation, print the string, run
-- the IO operation, print the string again, and finally return what
-- the operation returned.
--
-- Note! the operation should be run only once
--
-- Examples:
--   debug "CIAO" (return 3)
--     - prints two lines that contain CIAO
--     - returns the value 3
--   debug "BOOM" getLine
--     1. prints "BOOM"
--     2. reads a line from the user
--     3. prints "BOOM"
--     4. returns the line read from the user

debug :: String -> IO a -> IO a
debug s op = do
    putStrLn s
    thething <- op
    putStrLn s
    return thething