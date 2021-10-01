module Party where

import Data.List
import Data.Tree
import Employee

pretty :: GuestList -> String
pretty (GL employees fun) =
  "Total Fun: " ++
  show fun ++ "\n" ++ intercalate "\n" (sort (map empName employees))

main = do
  contents <- readFile "company.txt"
  putStr . pretty . maxFun . read $ contents
