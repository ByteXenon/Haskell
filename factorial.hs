factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

main :: IO ()
main = do
  let factorials = map factorial [1..]
  putStrLn $ unlines $ map show factorials
