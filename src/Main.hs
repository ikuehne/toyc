import System.Environment (getArgs)
import Text.Parsec ( parseTest )
import Parser
import Syntax ()

main = do [fname] <- getArgs
          contents <- readFile fname
          parseTest parse contents
