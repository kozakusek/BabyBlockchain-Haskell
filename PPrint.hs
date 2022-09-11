--bk418339
module PPrint where
import Data.List (intersperse)

writeln :: String -> IO ()
writeln = putStrLn

showsPair :: Show a => (String, a) -> ShowS
showsPair (k, v) = (k ++) . (": " ++) . shows v

pprH, pprV :: [ShowS] -> ShowS
pprV = intercalateS ('\n':)
pprH = intercalateS (' ':)

intercalateS :: ShowS -> [ShowS] -> ShowS
intercalateS = (foldl (.) id .) . intersperse 

pprListWith :: (a -> ShowS) -> [a] -> ShowS
pprListWith = (pprV .) . map

runShows :: ShowS -> IO ()
runShows = putStrLn . ($"")
