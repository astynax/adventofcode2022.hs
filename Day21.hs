{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Char

import Control.Monad.State
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Megaparsec

newtype Name = Name String deriving (Show, Eq, Ord)
data Expr = Expr Name Op Name deriving (Show)
data Op = Add | Sub | Mul | Div deriving (Show)

data Equation
  = EoE Equation    Op Equation
  | EoI Equation    Op Int
  | IoE Int         Op Equation
  | Const Int
  | X

type Monkey = (Name, Either Expr Int)

main :: IO ()
main = do
  monkeys <- decode <$> readFile "Day21.input"
  print $ evaluate monkeys  -- 276156919469632
  print $ solution2 monkeys -- 3441198826073

decode :: String -> [Monkey]
decode = tryParse (many $ monkeyP <* "\n") "input"

monkeyP :: Parser Monkey
monkeyP =
  (,) <$> nameP <*> (": " *> (Right <$> decimal <|> Left <$> exprP))
  where
    nameP = Name <$> some (satisfy isAlpha)
    exprP = Expr <$> nameP <*> opP <*> nameP
    opP =
      (Add <$ " + ") <|>
      (Sub <$ " - ") <|>
      (Mul <$ " * ") <|>
      (Div <$ " / ")

evaluate :: [Monkey] -> Int
evaluate monkeys =
  go (Map.fromList known) [] unknown Map.! Name "root"
  where
    known   = [(n, v) | (n, Right v) <- monkeys]
    unknown = [(n, e) | (n, Left e) <- monkeys]
    go :: Map Name Int -> [(Name, Expr)] -> [(Name, Expr)] -> Map Name Int
    go m [] [] = m
    go m rs [] = go m [] rs
    go m rs (r@(n, Expr a1 op a2) : ms) =
      case (,) <$> Map.lookup a1 m <*> Map.lookup a2 m of
        Nothing -> go m (r : rs) ms
        Just (v1, v2) ->
          go (Map.insert n (calc op v1 v2) m) rs ms

build :: [Monkey] -> (Equation, Equation)
build monkeys =
  ((,) <$> go e1 <*> go e2)
  `evalState` Map.singleton (Name "humn") X
  where
    (Left (Expr e1 _ e2), m) =
      let mm = Map.fromList monkeys
          k = Name "root"
      in (mm Map.! k, Map.delete k mm)
    go n =
      gets (Map.lookup n) >>= \case
        Just e  -> pure e
        Nothing ->
          case m Map.! n of
            Right x -> endWith $ Const x
            Left (Expr a1 op a2) -> do
              r1 <- go a1
              r2 <- go a2
              case (r1, r2) of
                (Const a, Const b) ->
                  endWith . Const $ calc op a b
                (Const a, e) ->
                  endWith $ IoE a op e
                (a, Const b) ->
                  endWith $ EoI a op b
                _ ->
                  endWith $ EoE r1 op r2  -- not present in the input
      where
        endWith e = e <$ modify (Map.insert n e)

solution2 :: [Monkey] -> Int
solution2 ms = let (e, Const r) = build ms in solve e r

solve :: Equation -> Int -> Int
solve X rhs = rhs
solve (EoI e op i) rhs = solve e (calc (inv op) rhs i)
solve (IoE i Add e) rhs = solve e (calc Sub rhs i)
solve (IoE i Mul e) rhs = solve e (calc Div rhs i)
solve (IoE i Sub e) rhs = solve e (calc Sub i rhs)
solve (IoE i Div e) rhs = solve e (calc Div i rhs)
solve e rhs = error $ "Unsolvable: " <> show e <> " = " <> show rhs

calc :: Op -> Int -> Int -> Int
calc op v1 v2 = case op of
  Add -> v1 + v2
  Sub -> v1 - v2
  Mul -> v1 * v2
  Div -> v1 `div` v2

inv :: Op -> Op
inv Add = Sub
inv Sub = Add
inv Mul = Div
inv Div = Mul

example :: [Monkey]
example = decode $ unlines
  [ "root: pppw + sjmn"
  , "dbpl: 5"
  , "cczh: sllz + lgvd"
  , "zczc: 2"
  , "ptdq: humn - dvpt"
  , "dvpt: 3"
  , "lfqf: 4"
  , "humn: 5"
  , "ljgn: 2"
  , "sjmn: drzm * dbpl"
  , "sllz: 4"
  , "pppw: cczh / lfqf"
  , "lgvd: ljgn * ptdq"
  , "drzm: hmdt - zczc"
  , "hmdt: 32"
  ]

showOp :: Op -> String
showOp = \case
  Add -> " + "
  Sub -> " - "
  Mul -> " * "
  Div -> " / "

instance Show Equation where
  show (EoE e1 op e2) = concat ["(", show e1, showOp op, show e2, ")"]
  show (IoE i  op e)  = concat ["(", show i, showOp op, show e, ")"]
  show (EoI e  op i)  = concat ["(", show e, showOp op, show i, ")"]
  show (Const i)      = show i
  show X              = "X"
