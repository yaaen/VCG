{-# LANGUAGE FlexibleContexts #-}
module Parser_nm where

-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec as Parsec

-- I am the error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

-- alias Parsec.parse for more concise usage in my examples:
parse
  :: Parsec.Stream s Identity t =>
     Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule text = Parsec.parse rule "(source)" text

test = parse (Parsec.count 4 Parsec.letter) "ahoythere"
test2 = parse (Parsec.manyTill Parsec.letter Parsec.digit) "hello12345"
test3 = parse (Parsec.manyTill Parsec.letter Parsec.digit) "12345"

test6 = parse (Parsec.oneOf (":!#$%&*+.\"/\"<=>?\"@\\\\^|-~_)")) "_"

symbol_name :: Parsec.ParsecT String () Identity String
symbol_name = Parsec.many $
              Parsec.try $
              Parsec.letter <|> Parsec.digit <|> Parsec.oneOf (":!#$%&*+.\"/\"<=>?\"@\\\\^|-~_)")

test7 = parse symbol_name "b_@j_"

line2 :: Parsec.Parsec String () (String,String, String)
line2 = do
        Parsec.spaces
        sym_type <- Parsec.many1 Parsec.letter
        Parsec.spaces
        sym_name <- symbol_name
        return ("", sym_type, sym_name)

line3 :: Parsec.Parsec String () (String,String, String)
line3 = do
        sym_value <- Parsec.count 8 Parsec.alphaNum
        Parsec.spaces
        sym_type <- Parsec.many1 Parsec.letter
        Parsec.spaces
        sym_name <- symbol_name
        return (sym_value, sym_type, sym_name)

test9 = Parsec.many1 (do { Parsec.try (line3 <|> line2) >>= \line -> Parsec.char '\n' >> return line} )

test8 = parse test9 "08048610 T __x86.get_pc_thunk.bx"

helloOrHowdy :: Parsec.Parsec String () String
helloOrHowdy = do
    first <- Parsec.char 'h'
    rest <- Parsec.string "ello" <|> Parsec.string "owdy"
    return (first:rest)

-- This looks for letters, then spaces, then digits.
-- we then return letters and digits in a tuple.
myParser :: Parsec.Parsec String () (String,String)
myParser = do
    letters <- Parsec.many1 Parsec.letter
    Parsec.spaces
    digits <- Parsec.many1 Parsec.digit
    return (letters,digits)

myParser1 :: Parsec.ParsecT String () Identity (String,String)
myParser1 = myParser

myParser2 :: Parsec.Parsec String () (String,String)
myParser2 = myParser

test4 = parse myParser "hello 1000"
test5 = parse myParser "1000"

mySeparator :: Parsec.Parsec String () ()
mySeparator = do
    Parsec.spaces
    Parsec.char ','
    Parsec.spaces

myPairs :: Parsec.ParsecT String () Identity [(String, String)]
myPairs = Parsec.many (myParser >>= \pair -> mySeparator >> return pair)

-- I want to return a list of pairs as above but using a built in helper:
myPairs2a :: Parsec.Parsec String () [(String,String)]
myPairs2a = Parsec.sepBy myParser (Parsec.try (Parsec.eof <|> mySeparator <?> "a greeting!"))
