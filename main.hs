import Data.Char
import System.IO
import Control.Monad
import Data.List
import qualified Data.Text as Txt
import qualified Data.Text.IO as TIO

--Для вывода в консоль русских буквок
--import Text.Show.Unicode as UNI
--UNI.uprint lines_file   

{-
TODO 
Сделать проверку на экранирование * В процессе
Привести всё в хорошо 
Повысить читабльность кода В процессе ухудшения
Обработка других флагов
Интерфейс
Кастомные файлы
Оптимизация
-}


main :: IO ()
main = do

    fileRead <- openFile "test.md" ReadMode
    fileWrite <- openFile "test.html" WriteMode

    hSetEncoding fileRead utf8
    hSetEncoding fileWrite utf8

    putHeaderHtml fileWrite "Test"

    fileMD <- hGetContents fileRead
    let lines_file = lines fileMD

    print $ length lines_file

    forM_ [0..length lines_file-1] $ \i -> hPutStrLn fileWrite . remarkLine $ lines_file !! i

    putFooterHtml fileWrite
    hClose fileRead
    hClose fileWrite



putHeaderHtml :: Handle -> String -> IO ()
putHeaderHtml file fileName = do
    hPutStr file "<html>\n\t <head>\n\t\t<meta charset=\"utf-8\"> <meta http-equiv=\"Refresh\" content=\"5\" />"
    hPutStr file "\n\t\t<title>"
    hPutStr file fileName
    hPutStrLn file "</title>"

    hPutStrLn file "\t\t<style>\n\t\t\t.layer{"
    hPutStrLn file "\t\t\tbackground: #fff;"
    hPutStrLn file "\t\t\tborder: 2px solid black;"
    hPutStrLn file "\t\t\tpadding: 0 25% 0 25%;"
    hPutStrLn file "\t\t\t}\n\t\t</style>"

    hPutStrLn file "\t</head>\n\t<body>"
    hPutStrLn file "\t\t<div class = \"layer\">"
putFooterHtml :: Handle -> IO ()
putFooterHtml file = do
    hPutStrLn file "\t\t</div>"
    hPutStr file "\n\t</body>\n </html>"


remarkLine :: String -> String
remarkLine str
    | "######" `isPrefixOf` str = "\t\t<h6>" ++ strRemark (drop 6 str)  ++ "</h6> <hr>"
    | "#####" `isPrefixOf` str = "\t\t<h5>" ++ strRemark (drop 5 str) ++ "</h5> <hr>"
    | "####" `isPrefixOf` str = "\t\t<h4>" ++ strRemark (drop 4 str) ++ "</h4> <hr>"
    | "###" `isPrefixOf` str = "\t\t<h3>" ++ strRemark (drop 3 str) ++ "</h3> <hr>"
    | "##" `isPrefixOf` str = "\t\t<h2>" ++ strRemark (drop 2 str) ++ "</h2> <hr>"
    | "#" `isPrefixOf` str = "\t\t<h1>" ++ strRemark (drop 1 str) ++ "</h1> <hr>"
    | not $ null str =  "\t\t\t<p>" ++ strRemark str ++ "</p>"
    | otherwise = str
    where
        strRemark = remarkStEm




remarkStEm :: String -> String
remarkStEm str | "**" `isInfixOf` str = recur (words str) [] False False False
                   | otherwise = str
    where
        recur :: [String] -> String -> Bool -> Bool -> Bool -> String
        recur wordOfStr res flagEkran strongOpen emOpen
        ---Для Стронг
                        |not emOpen && not (null wordOfStr) && length (filter (=='*') headCur) == length headCur = recur (drop 1 wordOfStr) (res ++ " " ++ headCur) flagEkran  strongOpen emOpen --Для слова только из звёзд

                        |not emOpen && isPrefixOf "**" headCur && (isSuffixOf "**" headCur || isSuffixOf "**." headCur || isSuffixOf "**!" headCur || isSuffixOf "**?" headCur || isSuffixOf "**," headCur) = recur (drop 1 wordOfStr) (res ++ " <strong>" ++ takeWhile (/='*') (drop 2 headCur) ++ "</strong>") flagEkran strongOpen emOpen --Если в слове и префикс и суффикс

                        |not emOpen && isPrefixOf "\\*\\*" headCur && not (isSuffixOf "**" headCur || isSuffixOf "**." headCur || isSuffixOf "**!" headCur || isSuffixOf "**?" headCur || isSuffixOf "**," headCur) = recur (drop 1 wordOfStr) (res ++ " " ++ headCur) True strongOpen emOpen --Если в слове ЭкранПрефикс

                        |not emOpen && isPrefixOf "**" headCur = recur (drop 1 wordOfStr) (res ++ " <strong>" ++ drop 2 headCur) flagEkran True emOpen --Если в слове префикс

                        |not emOpen && (isSuffixOf "**" headCur || isSuffixOf "**." headCur || isSuffixOf "**!" headCur || isSuffixOf "**?" headCur || isSuffixOf "**," headCur) && flagEkran = recur (drop 1 wordOfStr) (res ++ " " ++ headCur) False strongOpen emOpen --Если экран открыт

                        |not emOpen && (isSuffixOf "**" headCur || isSuffixOf "**." headCur || isSuffixOf "**!" headCur || isSuffixOf "**?" headCur || isSuffixOf "**," headCur) && not ("\\*\\*" `isPrefixOf` headCur || "\\*" `isPrefixOf` headCur) = recur (drop 1 wordOfStr) (res ++ " " ++ take (length headCur -2) headCur ++ "</strong>") flagEkran False emOpen --Если в слове суффикс

        ---Для курсива
                        |not strongOpen && isPrefixOf "*" headCur && (isSuffixOf "*" headCur || isSuffixOf "*." headCur || isSuffixOf "*!" headCur || isSuffixOf "*?" headCur || isSuffixOf "*," headCur) = recur (drop 1 wordOfStr) (res ++ " <em>" ++ takeWhile (/='*') (drop 1 headCur) ++ "</em>") flagEkran  strongOpen emOpen --Если в слове и префикс и суффикс

                        |not strongOpen && isPrefixOf "\\**" headCur && (isSuffixOf "*" headCur || isSuffixOf "*." headCur || isSuffixOf "*!" headCur || isSuffixOf "*?" headCur || isSuffixOf "*," headCur) = recur (drop 1 wordOfStr) (res ++ " <em>" ++ takeWhile (/='*') (drop 3 headCur) ++ "</em>") flagEkran  strongOpen emOpen--Если в слове и префикс и суффикс

                        |not strongOpen && isPrefixOf "\\*" headCur && not("\\**" `isPrefixOf` headCur) && not (isSuffixOf "*" headCur || isSuffixOf "*." headCur || isSuffixOf "*!" headCur || isSuffixOf "*?" headCur || isSuffixOf "*," headCur) = recur (drop 1 wordOfStr) (res ++ " " ++ drop 1 headCur) True strongOpen True --Если в слове ЭкранПрефикс

                        |not strongOpen && isPrefixOf "*" headCur = recur (drop 1 wordOfStr) (res ++ " <em>" ++ drop 1 headCur) flagEkran  strongOpen True --Если в слове префикс

                        |not strongOpen && isPrefixOf "\\**" headCur = recur (drop 1 wordOfStr) (res ++ " <em>" ++ drop 3 headCur) flagEkran  strongOpen False --Если в слове Экранпрефикс

                        |not strongOpen && (isSuffixOf "*" headCur || isSuffixOf "*." headCur || isSuffixOf "*!" headCur || isSuffixOf "*?" headCur || isSuffixOf "*," headCur) && flagEkran = recur (drop 1 wordOfStr) (res ++ " " ++ headCur) False strongOpen emOpen -- Если экран открыт

                        |not strongOpen && (isSuffixOf "**" headCur || isSuffixOf "**." headCur || isSuffixOf "**!" headCur || isSuffixOf "**?" headCur || isSuffixOf "**," headCur) && not ("\\*" `isPrefixOf` headCur) = recur (drop 1 wordOfStr) (res ++ " " ++ takeWhile (/='*') headCur ++ "</em>") flagEkran strongOpen False --Если экран открыт

                        |not strongOpen && (isSuffixOf "*" headCur || isSuffixOf "*." headCur || isSuffixOf "*!" headCur || isSuffixOf "*?" headCur || isSuffixOf "*," headCur) && not ("\\*" `isPrefixOf` headCur) = recur (drop 1 wordOfStr) (res ++ " " ++ take (length headCur -1) headCur ++ "</em>") flagEkran  strongOpen False --Если в слове суффикс 


                        | not $ null wordOfStr && isPrefixOf "\\*" headCur && isSuffixOf "*" headCur = recur (drop 1 wordOfStr) (res ++ " " ++ drop 1 (take (length headCur -1) headCur)) flagEkran strongOpen emOpen

                        | not $ null wordOfStr = recur (drop 1 wordOfStr) (res ++ " " ++ headCur) flagEkran  strongOpen emOpen --Если ничего криминального
                        | otherwise = res --Если кончилось
                where headCur | null wordOfStr = ""
                              | otherwise = head wordOfStr






{-
Кладбище кода
remarkEmV2 :: String -> String
remarkEmV2 str | isInfixOf "*" str = recur (words str) []
               | otherwise = str
    where
        recur :: [String] -> String -> String
        recur wordOfStr res
                        | not (null wordOfStr) && (length (filter (=='*') headCur) == length headCur) = recur (drop 1 wordOfStr) (res ++ " " ++ headCur)  --Для слова только из звёзд

            where headCur | null wordOfStr = ""
                        | otherwise = head wordOfStr


remarkStrong :: String -> String
remarkStrong str | isInfixOf "**" str = recur str [] False False
                 | otherwise = str
    where
        recur :: String -> String  -> Bool -> Bool -> String
        recur str res flagTwo flagPass
                        | isPrefixOf "\\*\\*" curSym = recur (drop 4 str) (res ++ "**") flagTwo True
                        | isPrefixOf "\\**" curSym = recur (drop 3 str) (res ++ "**") flagTwo True
                        | isPrefixOf "**" curSym && flagPass = recur (drop 2 str) (res ++ [head str]) flagTwo False

                        | isPrefixOf "**" curSym && not flagTwo = recur (drop 2 str) (res ++ "<strong>") True flagPass
                        | isPrefixOf "**" curSym && flagTwo = recur (drop 2 str) (res ++ "</strong>") False flagPass
                        | not $ null str = recur (drop 1 str) (res ++ [head str]) flagTwo flagPass
                        | otherwise = res
            where
                curSym = take 4 str
remarksEm :: String -> String
remarksEm str | isInfixOf "*" str = recur str [] False False
              | otherwise  = str
    where
        recur :: String -> String -> Bool -> Bool -> String
        recur str res flagOne flagPass

                        | not $ null str = recur (drop 4 str) (res ++ curSym) flagOne flagPass
                        | otherwise = res
            where
                curSym = take 4 str
-}