-- |
module Dictionary 
( Dictionary
, Word
, dict_new
, dict_add
, dict_contains
, dict_load
, dict_save
) where

import System.IO()
import System.IO.Error

type Dictionary = [Word]
type Word = String

dict_new :: Dictionary
dict_new = []

dict_add :: Word -> Dictionary -> Dictionary
dict_add word dict = dict ++ [word]

dict_contains :: Word -> Dictionary -> Bool
dict_contains word dict = elem word dict

dict_load :: FilePath -> IO Dictionary
dict_load filename = do
                contents <- readFile $ filename
                return(lines contents)
                 -- Usar monada para que sea como una especie de imperativo
                -- signo $ obliga a que se haga readFile openfile
                -- foo -> bar el foo debe ser del tipo IO, porque IO es monada => foo tendra que ser monada para todo 
                -- foo
dict_save :: FilePath -> Dictionary -> IO ()
dict_save filename dict = catchIOError
                (do
                writeFile filename $ unlines dict)
                (\_ -> return())
