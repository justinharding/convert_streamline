import Data.Char (isSpace)

type Transaction = (String, String, String, String)

main :: IO ()
main = do
  fileContent <- readFile "test.journal"
  -- let content = filter (isNotCommentOrBlank) (map trim (lines fileContent))
  let transactions = splitIntoTransactions isFirstLineOfTransaction $ lines fileContent

  mapM_ (mapM_ (prettyPrint "%%%")) transactions


  -- print content
  -- print (intercalate "\nxx" (takeEvery 4 1 content))
  -- mapM_ putStrLn (takeEvery 4 1 content)
  -- mapM_ (prettyPrint "***") (takeEvery 3 1 content)
  -- mapM_ (mapM_ (prettyPrint "%%%")) (splitEvery 3 content)
  -- mapM_ ((prettyPrint "$$$") . head) (splitEvery 3 content)


takeEvery :: (Eq a, Num a, Enum a) => a -> a -> [b] -> [b]
takeEvery n offset = map snd . filter ((==offset) . fst) . zip (cycle[1..n])

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as, bs) = splitAt n xs

-- takeAllIndentedOrBlank :: [String] -> [[String]]
-- takeAllIndentedOrBlank [] = []
-- takeAllIndentedOrBlank (x:xs)
--                   | isFirstLineOfTransaction x = [x] : takeAllIndentedOrBlank xs

splitIntoTransactions :: (a -> Bool) -> [a] -> [[a]]
splitIntoTransactions predicate [] = []
splitIntoTransactions predicate (x:xs) = go [x] xs
  where go acc [] = [acc]
        go acc (y:ys) | predicate y = acc : go [y] ys
                      | otherwise = go (acc++[y]) ys

intercalate :: [a] -> [[a]] -> [a]
-- intercalate xs xss = concat (intersperse xs xss)
intercalate sep l = drop (length sep) $ concat $ map (\w -> sep ++ w) l

prettyPrint :: String -> String -> IO ()
prettyPrint decoration s = putStrLn (decoration ++ s ++ decoration)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

startsWith :: Char -> String -> Bool
startsWith _ [] = False
startsWith c (x:xs) = x == c

isNotCommentOrBlank :: String -> Bool
isNotCommentOrBlank s = not (null s) && not(startsWith ';' s)

isBlank :: String -> Bool
isBlank s = all isSpace s

isFirstLineOfTransaction :: String -> Bool
isFirstLineOfTransaction [] = False
isFirstLineOfTransaction (x:xs) = not $ elem x " ;"

