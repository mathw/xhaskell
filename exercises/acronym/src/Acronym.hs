module Acronym (abbreviate, shouldSplit, SplitVerdict(..), revrev, doSplit', doSplit, notEmpty) where

import Data.Char (isUpper, toUpper, isAlpha)

abbreviate :: String -> String
abbreviate xs = map toUpper $ map head $ doSplit shouldSplit xs

data SplitVerdict = Gather | Discard | Split
  deriving (Show, Eq)

-- |Given a character, should we include it in the current word, throw it away as a delimiter, or split just before it?
shouldSplit :: Char -> SplitVerdict
shouldSplit ' ' = Discard
shouldSplit '-' = Discard
shouldSplit c | isUpper c = Split
              | isAlpha c = Gather
              | otherwise = Discard

-- |Given a splitting function, split a list, discarding empty segments
doSplit :: (Char -> SplitVerdict) -> [Char] -> [[Char]]
doSplit f xs = revrev $ filter notEmpty $ doSplit' f [] [] xs

-- |This function builds a list of backwards words which is backwards, with lots of empty strings littered around
-- that's not deliberate, it's just how things work here, and doesn't really matter
doSplit' :: (Char -> SplitVerdict) -- ^Split decision function
         -> [Char]                 -- ^Accumulator - current word so far (backwards)
         -> [[Char]]               -- ^Current list of words we've found (also backwards)
         -> [Char]                 -- ^Current input
         -> [[Char]]
doSplit' f acc ws [] -- at the end of the input string
    | notEmpty acc = (acc:ws) -- if we have a word being built, stick it on the word list
    | otherwise    = ws -- just return the word list
doSplit' f acc ws (x:xs) =
    case f x of
        Gather  -> doSplit' f (x:acc) ws xs -- include this character in the currently-building word
        Discard -> doSplit' f [] (acc:ws) xs -- this character is a delimiter. Save the current word and start a new one.
        Split   -> -- this character is a split boundary just before
            case ws of -- however, all-caps words don't work with this
                [] -> doSplit' f [x] (acc:ws) xs -- if there are no found words, just drop the accumulator into the word list and start a new word
                (w:ws) | isAcronym w -> doSplit' f [] (([x] ++ acc ++ w):ws) xs -- if the most recently acquired word is an acronym, add this letter to it
                       | otherwise   -> doSplit' f [x] (acc:w:ws) xs -- otherwise, we're starting a new word

-- |Reverse a list of lists and each element thereof
revrev :: [[a]] -> [[a]]
revrev xs = reverse $ map reverse xs

-- |Is a list not empty?
notEmpty :: [a] -> Bool
notEmpty = not . null

isAcronym :: [Char] -> Bool
isAcronym xs = notEmpty xs && all isUpper xs