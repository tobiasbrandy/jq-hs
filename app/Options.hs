module Options (
  Options (..)
, getOptions

, Indent (..)

, Color (..)

, FilterInput (..)
) where

import Prelude hiding (seq)

import Options.Applicative (
  Parser, switch, flag', strOption, infoOption, option, auto, strArgument, long, short, help, metavar,
  ParserInfo, info, helper, fullDesc, progDesc, header, failureCode,
  ParserPrefs, customExecParser, prefs, showHelpOnEmpty, columns
  )

import Control.Applicative ((<|>), (<**>), many)
import System.IO (hIsTerminalDevice, stdout)
import Data.Text (Text)

jqhsVersion :: String
jqhsVersion = "1.0"

data Indent
  = Tab
  | Spaces Int
  deriving (Eq, Show)

data Color
  = CDefault Bool
  | CEnabled
  | CDisabled
  deriving (Eq, Show)

colorSetDefault :: Bool -> Color -> Color
colorSetDefault b (CDefault _) = CDefault b
colorSetDefault _ c = c

data FilterInput
  = Arg Text
  | File FilePath
  deriving (Eq, Show)

data Options = Options
  { slurp       :: Bool 
  , nullInput   :: Bool
  , indent      :: Indent 
  , colorOut    :: Color
  , sortKeys    :: Bool 
  , rawOut      :: Bool 
  , joinOut     :: Bool 
  , filterInput :: FilterInput
  , inputFiles  :: [FilePath]
  , exitStatus  :: Bool
} deriving (Eq, Show)

-- Run options parser and set missing defaults
-- - Color default: true if stdout is tty
getOptions :: IO Options
getOptions = do
  ret@Options { colorOut } <- customExecParser helpPrefs optionsParser
  isTty <- hIsTerminalDevice stdout
  return ret { colorOut = colorSetDefault isTty colorOut }

helpPrefs :: ParserPrefs
helpPrefs = prefs
  (  showHelpOnEmpty
  <> columns 110
  )

optionsParser :: ParserInfo Options
optionsParser = info (options <**> helper <**> renderVersion)
  ( fullDesc
  <> progDesc "\
      \\n\
      \jqhs is a haskell implementation of jq. jq is a tool for processing \
      \JSON inputs, applying the given filter to \
      \its JSON text inputs and producing the filter's results as JSON on \
      \standard output.\
      \\n\
      \The simplest filter is ., which copies jq's input to its output \
      \unmodified.\
      \\n\
      \For more advanced filters see the jq(1) manpage (\"man jq\") \
      \and/or https://stedolan.github.io/jq\
      \\n\
      \Example:\
      \\n\
      \$ echo '{\"foo\": 0}' | jq '.foo += 1' => {\"foo\": 1}\
      \"
  <> header ("jqhs - haskell implementation of the jq commandline JSON processor [version " <> jqhsVersion <> "]")
  <> failureCode 2
  )

renderVersion :: Parser (a -> a)
renderVersion = infoOption ("jqhs-" <> jqhsVersion)
  (  long "version"
  <> help "Output the jq version and exit with zero."
  )

options :: Parser Options
options = do
  slurp       <- slurpArg
  nullInput   <- nullInputArg
  indent      <- indentArg
  colorOut    <- colorOutArg
  sortKeys    <- sortKeysArg
  rawOut      <- rawOutArg
  joinOut     <- joinOutArg
  filterInput <- filterInputArg
  inputFiles  <- inputFilesArg
  exitStatus  <- exitStatusArg
  return Options {..}

slurpArg :: Parser Bool
slurpArg = switch
  (  long "slurp"
  <> short 's'
  <> help "\
      \Instead of running the filter for each JSON object in the input, read the entire input stream\
      \ into a large array and run the filter just once.\
      \"
  )

nullInputArg :: Parser Bool
nullInputArg = switch
  (  long "null-input"
  <> short 'n'
  <> help "\
      \Don\'t read any input at all! Instead, the filter is run once using null as the input. This is\
      \ useful when using jq as a simple calculator or to construct JSON data from scratch.\
      \"
  )

indentArg :: Parser Indent
indentArg =
  flag' Tab
    (  long "tab"
    <> help "Use a tab for each indentation level instead of two spaces."
    )
  <|>
  Spaces <$> option auto
    (  long "indent"
    <> help "Use the given number of spaces (no more than 8) for indentation."
    <> metavar "n"
    )
  <|>
  flag' (Spaces 0)
    (  long "compact-output"
    <> short 'c'
    <> help "\
        \By default, jq pretty-prints JSON output. Using this option will result in more compact output\
        \ by instead putting each JSON object on a single line.\
        \"
    )
  <|>
  pure (Spaces 2)

colorOutArg :: Parser Color
colorOutArg =
  flag' CEnabled
    (  long "color-output"
    <> short 'C'
    <> help "\
        \Output colored JSON. This is the default if writing to a terminal.\n\
        \\n\
        \To configure alternative colors just set the JQ_COLORS environment variable to colon-delimited\
        \ list of partial terminal escape sequences like \"1;31\", in this order:\n\
        \null:false:true:numbers:strings:arrays:objects\n\
        \\n\
        \The default color scheme is the same as setting \"JQHS_COLORS=1;30:0;39:0;39:0;39:0;32:1;39:1;39\"\n\
        \\n\
        \For reference on VT100/ANSI escapes you may use the \"Set Display Attributes\" on\
        \ https://www2.ccs.neu.edu/research/gpc/VonaUtils/vona/terminal/vtansi.htm .\
        \"
    )
  <|>
  flag' CDisabled
    (  long "monochrome-output"
    <> short 'M'
    <> help "Disable colored JSON output, even when writing to a terminal."
    )
  <|>
  pure (CDefault False)

sortKeysArg :: Parser Bool
sortKeysArg = switch
  (  long "sort-keys"
  <> short 'S'
  <> help "Output the fields of each object with the keys in sorted order."
  )

rawOutArg :: Parser Bool
rawOutArg = switch
  (  long "raw-output"
  <> short 'r'
  <> help "\
      \With  this option, if the filter\'s result is a string then it will be written directly to standard\
      \ output rather than being formatted as a JSON string with quotes. This can be useful for making jq\
      \ filters talk to non-JSON-based systems.\
      \"
  )

joinOutArg :: Parser Bool
joinOutArg = switch
  (  long "join-output"
  <> short 'j'
  <> help "Like -r but jq won\'t print a newline after each output."
  )

filterInputArg :: Parser FilterInput
filterInputArg =
  File <$> strOption
    (  long "from-file filename"
    <> short 'f'
    <> help "\
      \Read filter from the file rather than from a command line, like awk\'s -f option.\
      \ You can also use \'#\' to make comments.\
      \"
    <> metavar "filename"
    )
  <|>
  Arg <$> strArgument
    (  help "JQ filter to execute."
    <> metavar "FILTER"
    )
  <|>
  pure (Arg "")

inputFilesArg :: Parser [FilePath]
inputFilesArg = many $ strArgument
  ( help "Json files to read"
  <> metavar "FILES..."
  )

exitStatusArg :: Parser Bool
exitStatusArg = switch
  (  long "exit-status"
  <> short 'e'
  <> help "\
    \Sets the exit status of jq to 0 if the last output values was neither false nor null, 1 if the last\
    \ output value was either false or null, or 4 if no valid result was ever produced. Normally jq exits\
    \ with 2 if there was any usage problem or system error, 3 if there was a jq program compile error,\
    \ or 0 if the jq program ran.\n\
    \\n\
    \Another way to set the exit status is with the halt_error builtin function.\
    \"
  )
