import Data.Char (isSpace)
import Data.List (isInfixOf)
import Data.List.Utils

type Transaction = (String, String, String, String)

main :: IO ()
main = do
  fileContent <- readFile "test.journal"
  let transactions = splitIntoTransactions isFirstLineOfTransaction $ lines fileContent
  -- mapM_ (printTransaction decorateLine decorateTransaction) transactions
  mapM_ (printTransaction "" (return ())) transactions
  let newTransactions = map (modifyTransaction "MITRE 10" "Expenses:Misc" "Expenses:Hardware") transactions
  -- mapM_ (printTransaction "" (return ())) newTransactions

  let dictionary = [("MITRE 10", "Expenses:Misc", "Expenses:Hardware"), ("TOKEN FEE", "Expenses:Misc", "Expenses:BankFee")]
  let newTransactions2 = map (doIt dictionary) transactions
  mapM_ (printTransaction "" (return ())) newTransactions2

doIt :: [(String, String, String)] -> [String] -> [String]
doIt dictionary transaction = foldr (\x y -> modifyTransactionWithTuple x transaction) transaction (filter (check transaction) dictionary)

modifyTransactionWithTuple :: (String, String, String) -> [String] -> [String]
modifyTransactionWithTuple tuple l = modifyTransaction (fst3 tuple) (snd3 tuple) (thd3 tuple) l

-- getTuple :: [(String, String, String)] -> (String, String, String)
check :: [String] -> (String, String, String) -> Bool
check x y = isInfixOf (fst3 y) (head x)

splitIntoTransactions :: (a -> Bool) -> [a] -> [[a]]
splitIntoTransactions predicate [] = []
splitIntoTransactions predicate (x:xs) = go [x] xs
  where go acc [] = [acc]
        go acc (y:ys) | predicate y = acc : go [y] ys
                      | otherwise = go (acc++[y]) ys

printTransaction :: Foldable t => String -> IO () -> t String -> IO ()
printTransaction decorateLine decorateTransaction transaction = do
  mapM_ (prettyPrint decorateLine) transaction
  decorateTransaction
  -- putStrLn ""

prettyPrint :: String -> String -> IO ()
prettyPrint decoration s = putStrLn (decoration ++ s ++ decoration)

modifyTransaction :: String -> String -> String -> [String] -> [String]
modifyTransaction search1 search2 replaceString (x:xs)
    | isInfixOf search1 x = x : modifyCategory search2 replaceString xs
    | otherwise = x : xs

modifyCategory :: String -> String -> [String] -> [String]
modifyCategory _ _ [] = []
modifyCategory searchString replaceString (x:xs)
    | isInfixOf searchString x = replace searchString replaceString x : xs
    | otherwise = x : modifyCategory searchString replaceString xs

isFirstLineOfTransaction :: String -> Bool
isFirstLineOfTransaction [] = False
isFirstLineOfTransaction (x:xs) = not $ elem x " ;"

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

decorateLine = "%%%"

decorateTransaction = putStrLn ""
