module Main
  ( main
  ) where

import           Data.List  (nub)
import           Data.Maybe (mapMaybe)


data OpCombo = OpCombo Op Op Op


data NumCombo = NumCombo Double Double Double Double


allNumCombinations :: [NumCombo]
allNumCombinations =
    [ NumCombo 1 1 5 8
    , NumCombo 1 1 8 5
    , NumCombo 1 5 1 8
    , NumCombo 1 8 1 5
    , NumCombo 1 5 8 1
    , NumCombo 1 8 5 1
    , NumCombo 5 1 1 8
    , NumCombo 8 1 1 5
    , NumCombo 5 1 8 1
    , NumCombo 8 1 5 1
    , NumCombo 5 8 1 1
    , NumCombo 8 5 1 1
    ]


data Op
  = Plus
  | Minus
  | Mult
  | Div
  deriving (Eq, Enum, Bounded)


instance Show Op where
  show Plus  = "+"
  show Minus = "-"
  show Mult  = "*"
  show Div   = "/"


opToFunc :: Op -> Double -> Double -> Double
opToFunc Plus  = (+)
opToFunc Minus = (-)
opToFunc Mult  = (*)
opToFunc Div   = (/)


allOps :: [Op]
allOps = [minBound ..]


allOpCombinations :: [OpCombo]
allOpCombinations =
  [ OpCombo a b c
  | a <- allOps
  , b <- allOps
  , c <- allOps
  ]


calc :: Double -> OpCombo -> NumCombo -> Maybe String
calc shouldBe (OpCombo op1 op2 op3) (NumCombo n1 n2 n3 n4) =
    let f1 = opToFunc op1
        f2 = opToFunc op2
        f3 = opToFunc op3
    in
    if n1 `f1` n2 `f2` n3 `f3` n4 == shouldBe then
        Just $
            show n1 ++ " " ++ show op1 ++ " " ++
            show n2 ++ " " ++ show op2 ++ " " ++
            show n3 ++ " " ++ show op3 ++ " " ++
            show n4 ++ " = " ++ show shouldBe
    else if (n1 `f1` n2) `f2` (n3 `f3` n4) == shouldBe then
        Just $
            "(" ++ show n1 ++ " " ++ show op1 ++ " " ++ show n2 ++ ") " ++
            show op2 ++ " " ++
            "(" ++ show n3 ++ " " ++ show op3 ++ " " ++ show n4 ++ ")" ++
            " = " ++ show shouldBe
    else if n1 `f1` (n2 `f2` n3) `f3` n4 == shouldBe then
        Just $
            show n1 ++ " " ++ show op1 ++
            " (" ++ show n2 ++ " " ++ show op2 ++ " " ++ show n3 ++ ") " ++
            show op3 ++ " " ++ show n4 ++ " = " ++ show shouldBe
    else if n1 `f1` (n2 `f2` (n3 `f3` n4)) == shouldBe then
        Just $
            show n1 ++ " " ++ show op1 ++ " (" ++ show n2 ++ " " ++ show op2 ++
            " (" ++ show n3 ++ " " ++ show op3 ++ " " ++ show n4 ++ "))" ++
            " = " ++ show shouldBe
    else
        Nothing


solve :: [String]
solve =
    nub $
        concatMap
        (\numCombo ->
            mapMaybe
                (\opCombo -> calc 10 opCombo numCombo)
            allOpCombinations)
        allNumCombinations


main :: IO ()
main = print solve
