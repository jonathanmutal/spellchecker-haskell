
{-# LANGUAGE DoAndIfThenElse #-}
-- |

module Document 
( Document
, doc_open
, doc_close
, doc_get_word
, doc_put_word
) where

import System.IO
import Data.Char

type Word = String

data Document = Document { doc_in :: Handle
                         , doc_out :: Handle
                         }

-- | Abre los archivos especificados por los paths
-- pasados como argumentos. El primer path repre-
-- senta el archivo de entrada a procesar, y el
-- segundo path representa el archivo de salida
-- donde se guarda el documento ya procesado.
doc_open :: FilePath -> FilePath -> IO Document
doc_open fp1 fp2 = do
                   file_input <- openFile fp1 ReadMode
                   file_output <- openFile fp2 WriteMode
                   return(Document {doc_in = file_input, doc_out = file_output})

doc_close :: Document -> IO ()
doc_close doc = do
                hClose $ doc_in doc  --LLamamos a doc_in en doc
                hClose $ doc_out doc -- $ como el @ de tipos en clase
                return ()

-- | Obtiene una palabra del documento especificado,
-- copiando todos los caracteres no alfabeticos
-- precedentes al archivo de salida.
-- Cuando alcanza el final del documento, lo señaliza
-- con una excepcion.
doc_get_word :: Document -> IO Word
doc_get_word doc = doc_get_word_1 doc []

doc_get_word_1 :: Document -> Word -> IO Word
doc_get_word_1 doc word = do
                          c <- hGetChar $ doc_in doc
                          if not $ isAlpha(c)
                            then do
                              if word == []
                                then do
                                  hPutChar (doc_out doc) c
                                  doc_get_word_1 doc word
                              else
                                do
                                  hSeek (doc_in doc) RelativeSeek (-1)
                                  return word
                          else
                            doc_get_word_1 doc (word ++ [c])

-- | Escribe una palabra en el documento de salida.
doc_put_word :: Word -> Document -> IO ()
doc_put_word word doc = do
                        hPutStr (doc_out doc) word
                        return ()
