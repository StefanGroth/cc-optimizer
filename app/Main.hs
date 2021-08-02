module Main (main) where

data Building = Building
  { name :: String,
    count :: Int,
    baseCost :: Rational,
    baseCPS :: Rational,
    mods :: [Rational -> Rational]
  }

times :: Num a => a -> (a -> a)
times x =(*x)

doubleN :: (Integral b, Num a) => b -> a -> a
doubleN n = times (2^n)

addPercentTo :: Num a => a -> a -> a
addPercentTo percent number = number + percent * number

applyAll :: [a -> a] -> a -> a
applyAll list a = foldl(\x a -> a x) a list

sugarLumps = addPercentTo


price :: Building -> Rational
price building = p * 1.15 ^ c
  where
    c = count building
    p = baseCost building

profit :: Building -> Rational
profit building = cps / p
    where   
            ms = mods building
            bcps = baseCPS building
            cps = applyAll ms bcps
            p = price building


grannyCount :: Int
grannyCount=133

grannyBonus=addPercentTo $ toRational grannyCount

buildings =
  [ Building "Cursor" 100 15 0.1 [doubleN 3],
    Building "Grandma" grannyCount 100 1 [doubleN 12],
    Building "Farm" 100 1100 8 [doubleN 5, grannyBonus],
    Building "Mine" 73 (1.2 * 10 ^ 4) 47 [doubleN 4, grannyBonus],
    Building "Factory" 66 (1.3 * 10 ^ 5) 260 [doubleN 4, grannyBonus],
    Building "Bank" 68 (1.4 * 10 ^ 6) 1400 [doubleN 4, sugarLumps 1, grannyBonus],
    Building "Temple" 50 (2 * 10 ^ 7) 7800 [doubleN 3, sugarLumps 1, grannyBonus],
    Building "Wizard Tower" 37 (3.3 * 10 ^ 8) (4.4*10^4) [doubleN 2, sugarLumps 1, grannyBonus],
    Building "Shipment" 18 (5.1 * 10 ^ 9) (2.6*10^5) [doubleN 1],
    Building "Alchemy Lab" 6 (7.5 * 10 ^ 10) (1.6*10^6) []
  ]

totalProfit :: [Building] -> Rational
totalProfit bs = sum $ map profit bs

main :: IO ()
main = print . snd . maximum $ map (\ building -> (profit building, name building) ) buildings
