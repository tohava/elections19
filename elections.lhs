> import Control.Arrow
> import Control.Monad
> import Data.List
> import Data.Char
> import System.IO
> import Debug.Trace
> import System.Exit

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
> percent f x = fromIntegral (f x) / fromIntegral (accepted x) * 100

-- The CSV uses windows-1255 encoding, we convert.

> toUnicode :: String -> String
> toUnicode x = map conv x
>   where 
>     conv = chr . conv' . ord
>     conv' x | 224 <= x && x <= 250 = x + (0x5D0 - 224) 
>             | otherwise = x

> compareWith f x y = compare (f x) (f y)

where is:
ג'ש
כאוכב אבו אל היג'ה
שגור


> arabs = ["אום אל-פחם",
>          "אל-עריאן",
>          "באקה אל-גרביה", 
>          "בסמ\"ה", 
>          "ג'סר א-זרקא", 
>          "כפר קרע", 
>          "מייסר", 
>          "מעלה עירון",
>          "עין חוד",
>          "ערערה",
>          "אבו גוש",
>          "עין נקובא",
>          "עין ראפה",
>          "ג'לג'וליה",
>          "זמר",
>          "טייבה",
>          "טירה",
>          "כפר ברא",
>          "כפר קאסם",
>          "אבו סנאן",
>          "אכסאל",
>          "אעבלין",
>          "בועיינה-נוג'ידאת",
>          "ג'דיידה-מכר",
>          "דבוריה",
>          "דחי",
>          "דייר חנא",
>          "זרזיר",
>          "חמאם",
>          "טורעאן",
>          "טמרה (יזרעאל)",
>          "טמרה",
>          "יפיע",
>          "כאבול",
>          "כעביה-טבאש-חג'אג'",
>          "כפר יאסיף",
>          "כפר כנא",
>          "כפר מנדא",
>          "כפר מצר",
>          "מוקייבלה",
>          "מזרעה",
>          "מגאר",
>          "משהד",
>          "נחף",
>          "ניין",
>          "נצרת",
>          "סולם",
>          "סח'נין",
>          "צנדלה",
>          "ע'ג'ר",
>          "עיילבון",
>          "עילוט",
>          "עין מאהל",
>          "עראבה",
>          "ראמה",
>          "רומאנה",
>          "ריינה",
>          "שייח' דנון",
>          "שעב",
>          "שפרעם"]

Main entrypoint
       
          
> main :: IO ()     
> main = do
>   hSetEncoding stdin latin1 -- So we can read the windows-1255 without crash
>   contents0 <- getContents
>   let contents1 = toUnicode contents0
>   let results = getResults contents1 :: [Line]
>   let poses = map pos results
>   unless (all (`elem` poses) arabs) $ do
>     putStrLn "Missing arabs: " 
>     mapM putStrLn $ flip filter arabs $ not . (`elem` poses)
>     exitFailure
>   forM [(shas, "ש\"ס"), 
>         (kadima, "קדימה"),
>         (otzma, "עוצמה"),
>         (likud, "ליכוד")] $
>     \(party,partyStr) -> do
>       putStrLn "-------"
>       putStrLn partyStr
>       putStrLn "-------"
>       forM (sortBy (compareWith $ percent party) $ 
>               flip filter results $ (`elem` arabs) . pos) $
>            \l@Line{pos=p} -> do 
>              putStrLn $ p ++ ": " ++ (show $ percent party l) ++ "% - " ++ (show $ party l)
>   return ()