> import Control.Arrow
> import Control.Monad
> import Data.List
> import Data.Char
> import System.IO
> import Debug.Trace

a line from the CSV

> data Line = Line { pos :: String,
>                    posShort :: String,                     
>                    allowed :: Int,
>                    voters :: Int,
>                    failed :: Int,
>                    accepted :: Int,
>                    avoda :: Int,
>                    yahadut :: Int,
>                    balad :: Int,
>                    moreshetAvot :: Int,
>                    haimBekavod :: Int,
>                    tikvaLeshinui :: Int,
>                    hadash :: Int,
>                    eretzHadasha :: Int,
>                    unknown :: Int,
>                    boneiHaaretz :: Int,
>                    baitYehudi :: Int,
>                    haIsraelim :: Int,
>                    kadima :: Int,
>                    likud :: Int,
>                    meretz :: Int,
>                    britOlam :: Int,
>                    or :: Int,
>                    otzma :: Int,
>                    mitkademet :: Int,
>                    raam :: Int,
>                    pirates :: Int,
>                    yeshAtid :: Int,
>                    koachLeashpia :: Int,
>                    kalkala :: Int,
>                    unknown2 :: Int,
>                    achim :: Int,
>                    kulanuHaverim :: Int,
>                    amShalem :: Int,
>                    hatnuaa :: Int,
>                    tzedek :: Int,
>                    daam :: Int,
>                    aleYarok :: Int,
>                    hayerukim :: Int,
>                    shas :: Int }

Helper function for splitting

> wordsWhen     :: (Char -> Bool) -> String -> [String]
> wordsWhen p s =  case dropWhile p s of
>                       "" -> []
>                       s' -> w : wordsWhen p s''
>                             where (w, s'') = break p s'

Convert file data to our data type

> getResults = map conv . filter (/="") . tail . map (filter (/='\r')) . lines
>   where 
>     conv x = 
>       let [ pos,
>             posShort,              
>             allowed ,
>             voters ,
>             failed ,
>             accepted ,
>             avoda ,
>             yahadut ,
>             balad ,
>             moreshetAvot ,
>             haimBekavod ,
>             tikvaLeshinui ,
>             hadash ,
>             eretzHadasha ,
>             unknown ,
>             boneiHaaretz ,
>             baitYehudi ,
>             haIsraelim ,
>             kadima ,
>             likud ,
>             meretz ,
>             britOlam ,
>             or ,
>             otzma ,
>             mitkademet ,
>             raam ,
>             pirates ,
>             yeshAtid ,
>             koachLeashpia ,
>             kalkala ,
>             unknown2 ,
>             achim ,
>             kulanuHaverim ,
>             amShalem ,
>             hatnuaa ,
>             tzedek ,
>             daam ,
>             aleYarok ,
>             hayerukim ,
>             shas ] = wordsWhen (==',') x
>       in Line pos posShort
>               (read allowed   )
>               (read voters    )
>               (read failed    )
>               (read accepted  )
>               (read avoda     )
>               (read yahadut   )
>               (read balad     )
>               (read moreshetAvot)
>               (read haimBekavod)
>               (read tikvaLeshinui)
>               (read hadash    )
>               (read eretzHadasha)
>               (read unknown   )
>               (read boneiHaaretz)
>               (read baitYehudi )
>               (read haIsraelim )
>               (read kadima    )
>               (read likud     )
>               (read meretz    )
>               (read britOlam  )
>               (read or        )
>               (read otzma     )
>               (read mitkademet )
>               (read raam      )
>               (read pirates   )
>               (read yeshAtid  )
>               (read koachLeashpia)
>               (read kalkala   )
>               (read unknown2  )
>               (read achim     )
>               (read kulanuHaverim)
>               (read amShalem  )
>               (read hatnuaa   )
>               (read tzedek    )
>               (read daam      )
>               (read aleYarok  )
>               (read hayerukim )
>               (read shas )
   
Creates a new function that returns what percent of people voted to a party

> percent :: Fractional a => (Line -> Int) -> (Line -> a)
> percent f x = fromIntegral (f x) / fromIntegral (accepted x)

-- The CSV uses windows-1255 encoding, we convert.

> toUnicode :: String -> String
> toUnicode x = map conv x
>   where 
>     conv = chr . conv' . ord
>     conv' x | 224 <= x && x <= 250 = x + (0x5D0 - 224) 
>             | otherwise = x

> compareWith f x y = compare (f x) (f y)

Main entrypoint
       
> main :: IO ()     
> main = do
>   hSetEncoding stdin latin1 -- So we can read the windows-1255 without crash
>   contents0 <- getContents
>   let contents1 = toUnicode contents0
>   let results = getResults contents1 :: [Line]
>   forM (sortBy (compareWith $ percent otzma) results) $
>     \l@Line{pos=p} -> putStrLn (p ++ " - " ++ (show $ percent otzma l))
>   return ()