module Main where

import Control.Applicative
import Data.Char

data CalcExpr = CalcNum Integer | CalcExpr {left :: CalcExpr, func :: CalcFunc, right :: CalcExpr} deriving (Show, Eq)
data CalcFunc = CalcMul | CalcDiv | CalcAdd | CalcSub deriving (Show, Eq)

newtype Parser a = Parser {
    runParser :: String -> Maybe (String, a)
}

instance Functor Parser where 
    fmap f (Parser p) = Parser $ \input -> do
        (input', a) <- p input
        Just (input', f a)

instance Applicative Parser where
    pure a = Parser $ \input -> Just (input,a)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (input', f) <- p1 input
        (input'', a) <- p2 input'
        Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser a) <|> (Parser b) = Parser $ \input -> a input <|> b input


spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> let (token, rest) = span f input
                                in Just (rest, token)

ws :: Parser String
ws = spanP isSpace

notNull :: Parser [a] -> Parser[a]
notNull (Parser p) = Parser $ \input -> do
    (input', xs) <- p input
    if null xs then Nothing else Just (input', xs)

calcNum :: Parser CalcExpr
calcNum = f <$> notNull (spanP isDigit)
    where f digits = CalcNum $ read digits

calcFunc :: Parser CalcFunc
calcFunc = ws *> (calcMul <|> calcDiv <|> calcAdd <|> calcSub) <* ws
        where 
          calcAdd = CalcAdd <$ charP '+'
          calcSub = CalcSub <$ charP '-'
          calcMul = CalcMul <$ charP '*'
          calcDiv = CalcDiv <$ charP '/'

calcComplexExpr :: Parser CalcExpr
calcComplexExpr = CalcExpr <$> (calcNum <|> empty) <*> calcFunc <*> calcExpr

charP :: Char -> Parser Char
charP c = Parser f
  where 
    f (x:xs)
      | x == c = Just(xs, c)
      | otherwise = Nothing
    f [] = Nothing

calcExpr :: Parser CalcExpr
calcExpr = calcComplexExpr <|> calcNum

main:: IO()
main = undefined

