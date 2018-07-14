import Data.Char (isSpace)

type Transaction = (String, String, String, String)

main :: IO ()
main = do
  fileContent <- readFile "test.journal"
  let content = filter (isCommentOrBlank) (map trim (lines fileContent))

  -- print content
  -- print (intercalate "\nxx" (takeEvery 4 1 content))
  -- mapM_ putStrLn (takeEvery 4 1 content)
  mapM_ (prettyPrint "***") (takeEvery 3 1 content)
  mapM_ (mapM_ (prettyPrint "%%%")) (splitEvery 3 content)
  mapM_ ((prettyPrint "$$$") . head) (splitEvery 3 content)


takeEvery :: (Eq a, Num a, Enum a) => a -> a -> [b] -> [b]
takeEvery n offset = map snd . filter ((==offset) . fst) . zip (cycle[1..n])

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs
  where (as, bs) = splitAt n xs

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

isCommentOrBlank :: String -> Bool
isCommentOrBlank s = not (null s) && not(startsWith ';' s)
-- isCommentOrBlank s = (not . null) s && (not . startsWith ';') s
-- isCommentOrBlank = ap ((&&) . not . null) (not . startsWith (';'))

