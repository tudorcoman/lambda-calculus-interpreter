
module REPLCommand where

import Lab2
import Control.Applicative (many, (<|>))

data REPLCommand
  = Quit
  | Load String
  | Eval String
  deriving (Show)

quit :: Parser REPLCommand
quit = (string ":quit" <|> string ":q") <* endOfInput *> return Quit

load :: Parser REPLCommand
load = (string ":l" <|> string ":load") *> many anychar >>= \s -> return (Load s)

eval :: Parser REPLCommand
eval = many anychar >>= \s -> return (Eval s)

replCommand :: Parser REPLCommand
replCommand = quit <|> load <|> eval

