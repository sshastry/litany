module Types where

-- TODO: include all languages supported by pandoc or pygments or whatnot.
data Language = Bash
              | Haskell
              | Scala
              | Python
              | Html
              | Css
              | CoffeeScript
              | JavaScript
              | Handlebars
              | Markdown
              | JSON
              | None
              deriving (Enum, Ord, Eq, Show)

languages = [Bash .. None]

data Delimiter = Begin | End | Pause | Continue | Nil deriving (Enum, Ord, Eq, Show)

delimiters = [Begin .. Nil]

data Fence = Fence { lineno :: Int
                   , delim :: Delimiter
                   , lang :: Language
                   , fname :: FilePath } deriving (Eq, Show)
