import Data.Char (isSpace)

type Transaction = (String, String, String, String)

main :: IO ()
main = do
  fileContent <- readFile "test.journal"
  let transactions = splitIntoTransactions isFirstLineOfTransaction $ lines fileContent
  -- mapM_ (printTransaction decorateLine decorateTransaction) transactions
  mapM_ (printTransaction "" (return ())) transactions
  let newTransactions = map (modifyTransaction "testValue" "testValue2" "newValue") transactions
  mapM_ (printTransaction "" (return ())) newTransactions

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
modifyTransaction t1 t2 r (x:xs)
    | x == t1 = x : modifyCategory t2 r xs
    | otherwise = x : xs

modifyCategory :: String -> String -> [String] -> [String]
modifyCategory _ _ [] = []
modifyCategory search replace (x:xs)
    | null x = []
    | x == search = replace : xs
    | otherwise = x : modifyCategory search replace xs

isFirstLineOfTransaction :: String -> Bool
isFirstLineOfTransaction [] = False
isFirstLineOfTransaction (x:xs) = not $ elem x " ;"

decorateLine = "%%%"

decorateTransaction = putStrLn ""
