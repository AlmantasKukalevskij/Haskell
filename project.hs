import Data.Char (ord) -- leidzia naudoti ascii ir pasitelkus tai galima konvertuoti chara i Int
import Text.Read -- naudojamas readMaybe [(Int, Int)] [(Char, Char)] funkcijoje tam, kad nenuluztu jeigu viena is ju nesuveiktu

widthLoop :: Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)] -- generateList dalis
widthLoop hei wid widCnt passed 
    | wid == widCnt = passed --grazina "passed" (sarasa kortezu) 
    | otherwise = widthLoop hei wid (widCnt+1) (passed++[(hei,widCnt)]) -- jeigu ne rekursiskai kvieciama funkcija su pakeistai parametrais

heightLoop :: Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)] -- generateList dalis
heightLoop hei wid heiCnt passed 
    | hei == heiCnt = passed--grazina "passed" (sarasa kortezu) 
    | otherwise = heightLoop hei wid (heiCnt+1) (passed++(widthLoop heiCnt wid 1 []))-- jeigu ne rekursiskai kvieciama funkcija su pakeistai parametrais

generateList :: Int -> Int -> [(Int, Int)] -- sukuria [(1,1),(1,2),(1,3),...] kortezu lista
generateList height width = heightLoop (height+1) (width+1) 1 [] -- iskvieciam heightLoop

changeToInt :: (Char,Char) -> (Int,Int) -- transformuojam (Char, Char) i (Int, Int)
changeToInt (one,two)
    | ord one > 96 && ord one < 123 = (ord one - 96, ord two - 96) -- tikrinam ascii reiksme (ord grazina Charo ascii reiksme), jeigu tai mazoji raide, suteikiam -96 
    | ord one > 64 && ord one < 91 = (ord one - 64, ord two -64)-- O jeigu tai didzioji raide, suteikiam -64
    | otherwise = (ord one - 48, ord two - 48) -- jeigu tai nera nei viena is auksciau esanciu, tai gali buti skaicius, todel suteikiam -48

createBaseList :: [(Int,Int)] -> [Int] -- sukuriam lista is 0 tokio dydzio kaip ir paduotas listas
createBaseList [] = [] -- stop condition
createBaseList (_:xs) = [0] ++ createBaseList xs -- lipdys sarasa kurio visos reiksmes 0, kol sarasas nera tuscias vykdoma rekursija

getPos :: (Int,Int) -> [(Int,Int)] -> Int -- grazina prozicija paduoto kortezo liste kur ieskom [(1,1),(1,2),(1,3),...]
getPos _ [] = 1 -- stop condition, grazina 1
getPos toFind (x:xs)
    | x == toFind = 1 -- jeigu x yra toks pat kortezas kaip ir paduotas, grazinam 1 ir loopas sustoja
    | otherwise = 1 + getPos toFind xs -- jeigu ne, grazinam 1 + rekursijos rezultata

replacePos :: [Int] -> Int -> Int -> [Int] -> [Int] -- pakeiciam intigeri duotoje pozicijoje i 1
replacePos [] _ _ passed = passed -- stop condition, jeigu paduotas listas yra tuscias (passed - nukopijuotas originalus listas)
replacePos (x:xs) pos cnt passed
    | pos == cnt = passed ++ [1] ++ xs -- jeigu randa pozicija tai grazina "passed" pridededa prie jo 1 ir prie jo prideda likusi xs sarasa
    | otherwise = replacePos xs pos (cnt+1) (passed ++ [x]) -- pasislenka per viena ir vykdoma rekursija

ff :: [(Int,Int)] -> [(Int,Int)] -> [Int] -> [Int] -- turim userio lista kortezu ir lista kortezu kur ieskome, pasitelkiant sia funkcija suzinome kur rasyti 1, o kur 0
ff [] _ exist = exist
ff (pos:rest) list exist = ff rest list (replacePos exist (getPos pos list) 1 []) -- rekursija atgal i ff su likusiais parametrais

ff1 :: [(Char,Char)] -> [(Int,Int)] -> [Int] -> [Int] -- turim userio lista kortezu ir lista kortezu kur ieskome, pasitelkiant sia funkcija suzinome kur rasyti 1, o kur 0
ff1 [] _ exist = exist
ff1 (pos:rest) list exist = ff1 rest list (replacePos exist (getPos (changeToInt pos) list) 1 [])-- rekursija atgal i ff su likusiais parametrais

divideList :: [Int] -> Int -> Int -> [Int] -> [[Int]] -> [[Int]] --Padalinam ta dideli lista su 0 ir 1 i mazesnius listus priklausomai nuo matricos dydzio
divideList [] _ _ a bigPassed = bigPassed ++ [a] -- stop condition, jeigu pirmas listas lieka nulis, priejom gala
divideList (x:xs) size cnt passed bigPassed 
     | size == cnt = divideList (x:xs) size 0 [] (bigPassed ++ [passed])-- uzdaromas vidinis listas
     | otherwise = divideList xs size (cnt+1) (passed++[x]) bigPassed -- tesiamas saraso sarasu formavimas rekursiskai

printList :: [[Int]] -> IO () --Printina lista
printList [] = return (); -- stop condition
printList (x:xs) = do -- jeigu listas ne tuscias do:
     print $ x --printina pirma iteracija padalinto listo
     printList xs -- rekursiskai paduodam kitas listo dalis kurias reikia atspausdinti

getRidMaybe :: Maybe a -> a
getRidMaybe (Just a) = a -- transformuojam bet kokia Maybe reiksme 

main :: IO ()
main = do
     putStrLn "Iveskite kortezus"--Enter tuples
     kortezaiChar <- getLine
     let kortezai = (readMaybe kortezaiChar :: Maybe [(Int, Int)])--bandoma ideti pradine reiksme i kortezu lista(grazina Maybe [(Int,Int)] jeigu veikia, jeigu ne tai nieko negrazina)
     let kortezaiC = (readMaybe kortezaiChar :: Maybe [(Char, Char)])--bandoma ideti pradine reiksme i kortezu lista(grazina Maybe [(Char,Char)] jeigu veikia, jeigu ne tai nieko negrazina

     putStrLn "Iveskite matricos ploti (kiek bus stulpeliu)"--Enter width of the matrix
     plotisChar <- getLine
     let plotis = (read plotisChar :: Int)--paverciam i inta

     putStrLn "Iveskite matricos auksti (kiek bus eiluciu)"--Enter height of the matrix
     aukstisChar <- getLine
     let aukstis = (read aukstisChar :: Int)--paverciam i inta

     if kortezai /= Nothing -- tikrinam ar pirma maybeRead suveike, jeigu reiksme nera niekas, ji suveike ir galime naudoti ff funkcija su intais
        then printList $ divideList (ff (getRidMaybe kortezai) (generateList aukstis plotis) (createBaseList (generateList aukstis plotis))) plotis 0 [] [] -- iskviecia ff funkcija su parametrais: (getRidMaybe kortezai kuri transformuoja Maybe [(Int, Int)] i [(Int, Int)] (generateList aukstis plotis sukuria lista kur ieskom [(1,1),(1,2),(1,3),(1,4),...]) (createBaseList (generateList aukstis plotis) kuris sukuria lista is 0 tokio dydzio kokio bus matrica) Tuomet printList atspausdina listu listus kad matrica graziai iseitu kokia ir turetu buti
        else if kortezaiC /= Nothing --Jeigu pirma maybeRead nesuveiki, tikrinam ar antra maybeRead suveike, ir jeigu ji grazina kazkokia reiksme tuomet galime naudoti ff1 funkcija su charais
            then printList $ divideList (ff1 (getRidMaybe kortezaiC) (generateList aukstis plotis) (createBaseList (generateList aukstis plotis))) plotis 0 [] [] -- tas pats kaip ir su ff, tik paduodam kortezaiC vietoje kortezai [(Char,Char)]
            else return ()
     putStrLn ""
