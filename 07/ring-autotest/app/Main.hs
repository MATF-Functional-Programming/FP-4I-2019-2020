module Main where

import System.Environment
import System.Process
import Data.List

import Debug.Trace

main :: IO ()
main = do { args <- getArgs    -- lista argumenata komandne linije
          ; mapM_ process args -- iteriramo i izvršavamo funkciju process za svaki argument komandne linije
          -- ; forM_ args process -- alternativa iz Control.Monad paketa
          -- for (auto &&arg: args) process(arg)
          }

process :: String -> IO ()
process file = do { contents <- readFile file     
                  ; let tests = getTests contents
                  ; if null tests then putStrLn $ file ++ ": nothing to test"
                    else do executeTests file tests
                  }

executeTests :: String -> [String] -> IO ()
executeTests file tests = 
    do { writeFile "script" $ unlines $
            [":set prompt \"\ESC[32m\""] ++ -- potpuno bespotrebno
            ["putStrLn \"\\nLoaded GHCi.\""] ++ -- potpuno bespotrebno
            concatMap (makeTest file) tests
       ; system $ "stack ghci ring-autotest:ring-autotest-test < script"
       ; return ()
       }

getTests :: String -> [String]
getTests contents = nub 
                  $ filter ("prop_" `isPrefixOf`) 
                  $ map (fst . head . lex) 
                  $ lines contents

makeTest :: String -> String -> [String]
makeTest file test = ["putStr \"" ++ test ++ "\\t\"", "quickCheck " ++ test]


-- alternativa ako su biblioteke instalirane globalno

-- executeTests :: String -> [String] -> IO ()
-- executeTests file tests =
--                do { writeFile "script" $ unlines $
--                      [":l " ++ file] ++
--                      [":m Test.QuickCheck"] ++
--                      concatMap (makeTest file) tests
--                   ; system $ "ghci -v0 < script"
--                   ; return ()
--                   }
-- makeTest :: String -> String -> [String]
-- makeTest file test = ["putStr \"" ++ test ++ ": \"", "quickCheck " ++ moduleName ++ "." ++ test]
                    --  where moduleName = drop (if "src/" `isPrefixOf` file then 4 else 0) $ takeWhile (/='.') file
