import System.IO

main :: IO ()
main = do
  input <- getLine -- Les inn en linje til en streng

  let n = read input :: Int -- Konverter linjen til nøyaktig 1 int
  let [n1, n2] = map read $ words input :: [Int] -- Konverter linjen til nøyaktig 2 ints
  print n1
  let numbers = map read $ words input :: [Int] -- Konvert linjen til n ints
      
  -- Samme koden fungerer også med andre typer, f.eks. Double:
  let doubles = map read $ words input :: [Double] -- Konvert linjen til n doubles
      
  putStrLn "Hei verden!" -- Print en streng på egen linje
  
  print n -- Print n på sin egen linje
  print (n1 + n2) -- Print resultatet av n1 + n2
  -- Husk: Bruk print til alt unntatt strings
 
  -- For mer kompliserte uttrykk, bruk show og ++ til å sette sammen en streng
  putStrLn ("Svar: " ++ show (n1 + n2))

  putStrLn $ unwords $ map show numbers -- Skriv ut liste med tall, separert med mellomrom

  -- For å sjekke om input er ferdig:
  finished <- isEOF -- (Krever import System.IO)
  if finished then return ()
    else do
    
    allInput <- getContents -- Les inn *all* inputten til en streng

    let inputLines = lines allInput -- Splitt i linjer
    
    putStrLn $ head $ inputLines -- Skriv ut kun den første linjen
    mapM putStrLn inputLines -- Skriv ut alle linjene

    putStrLn "Lykke til!"
