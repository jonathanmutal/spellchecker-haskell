{-# LANGUAGE DoAndIfThenElse #-}
-- |

module SpellChecker (do_spellcheck) where

import Document
import Dictionary
import CommandLine
import System.IO.Error

-- | La funcion 'do_spellcheck' es la funcion que se encarga de manejar
-- el proceso de chequeo ortografico. Esto incluye, cargar el diccionario,
-- abrir el archivo a procesar, procesar el archivo y luego guardar el
-- diccionario y el archivo de entrada ya procesado.
-- Toma como argumento los argumentos de linea de comando de tipo 'Params'.
do_spellcheck :: Params -> IO ()
do_spellcheck params = let
                      dict_ignored = dict_new
                    in
                    do
                      dict_main <- dict_load (dictionary params)
                      doc <- doc_open (filename params) "out.txt"
                      dictm <- process_document doc dict_main dict_ignored
                      doc_close doc
                      dict_save (dictionary params) dictm
                      putStrLn ("\nThe out file is in out.txt")

-- | La funcion 'process_document' ejecuta el proceso de chequeo ortografico.
-- Para ello, procesa el archivo palabra por palabra, copiandolas al archivo
-- de salida y consultando al usuario sobre que accion realizar ante una
-- palabra desconocida.
-- Cuando se termina de procesar el archivo, lo cual es señalizado mediante
-- una excepcion por 'doc_get_word', retorna el diccionario (el cual puede
-- haber sido modificado) para guardarlo.
process_document :: Document -> Dictionary -> Dictionary -> IO Dictionary
process_document doc dict_main dict_ignored = catchIOError
                                            (do
                                            word <- doc_get_word doc
                                            if (dict_contains word dict_main || dict_contains word dict_ignored)
                                              then
                                                do
                                                  doc_put_word word doc
                                                  dictm <- process_document doc dict_main dict_ignored
                                                  return dictm
                                            else
                                              do
                                                (word1, dictm, dicti) <- consult_user word dict_main dict_ignored
                                                doc_put_word word1 doc
                                                dictm1 <- process_document doc dictm dicti
                                                return dictm1)
                                                (\_ -> return dict_main)


-- | Verifica si una palabra es conocida, en cuyo caso, continua
-- con el procesamiento del archivo, sin realizar ninguna accion.
-- Si la palabra no es conocida, pregunta al usuario que accion
-- realizar con la misma. Las acciones pueden ser aceptar, ignorar
-- o reemplazar.
consult_user ::  Word -> Dictionary -> Dictionary -> IO (Word, Dictionary, Dictionary)
consult_user word dict_main dict_ignored = do
                putStrLn ("Word unkown: " ++ word ++ "\nWhat do you want to do: \n" ++ "Accept - (a) Ignore - (i) Replace - (r)")
                answer <- getLine
                case answer of ['a'] -> do {return (word, dict_add word dict_main, dict_ignored)}
                               ['i'] -> do {return (word, dict_main, dict_add word dict_ignored)}
                               ['r'] -> do {putStrLn("\nPlease enter the word:");
                                          str <- getLine;
                                          return (str, dict_main, dict_ignored)}
                               (_) -> do {putStrLn("\nCommand not found: " ++ answer);
                                          putStrLn("Enter the command again please:");
                                          consult_user word dict_main dict_ignored}

