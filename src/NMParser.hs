{-# LANGUAGE FlexibleContexts #-}
module NMParser where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<?>))
import Control.Applicative
import Control.Monad.Identity (Identity)

parse
  :: Parsec.Stream s Identity t =>
     Parsec.Parsec s () a -> s -> Either Parsec.ParseError a
parse rule text = Parsec.parse rule "(source)" text

symbol_name :: Parsec.ParsecT String () Identity String
symbol_name = Parsec.many $
              Parsec.try $
              Parsec.letter <|> Parsec.digit <|> Parsec.oneOf (":!#$%&*+.\"/\"<=>?\"@\\\\^|-~_)")

test1 = parse symbol_name "b_@j_"

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

nmParser = Parsec.many1 (do { Parsec.try (line3 <|> line2) >>= \line -> Parsec.char '\n' >> return line} )

test2 = parse nmParser "08048610 T __x86.get_pc_thunk.bx"
