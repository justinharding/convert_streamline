import Data.Char (isSpace)

type Transaction = (String, String, String, String)

main :: IO ()
main = do
  fileContent <- readFile "test.journal"
  let transactions = splitIntoTransactions isFirstLineOfTransaction $ lines fileContent
  -- mapM_ (printTransaction decorateLine decorateTransaction) transactions
  mapM_ (printTransaction "" (return ())) transactions

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

isFirstLineOfTransaction :: String -> Bool
isFirstLineOfTransaction [] = False
isFirstLineOfTransaction (x:xs) = not $ elem x " ;"

decorateLine = "%%%"

decorateTransaction = putStrLn ""
