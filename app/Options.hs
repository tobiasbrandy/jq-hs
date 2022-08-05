module Options (
  Options (..),
  getOptions
) where

import Control.Applicative ((<|>), (<**>), optional)
import Options.Applicative (
  Parser, switch, flag', strOption, infoOption, option, auto, strArgument, long, short, help, metavar,
  ParserInfo, info, helper, fullDesc, progDesc, header, failureCode,
  ParserPrefs, customExecParser, prefs, showHelpOnEmpty, columns
  )
import Data.ByteString (ByteString)

jqhsVersion :: String
jqhsVersion = "1.0"

data Indent
  = Tab
  | Space Int
  deriving (Eq, Show)

data Color
  = CDefault
  | CEnabled
  | CDisabled
  deriving (Eq, Show)

data FilterInput
  = Arg ByteString
  | File FilePath
  deriving (Eq, Show)

-- data ArgFile = ArgFile Text FilePath deriving (Eq, Show)

data Options = Options {
  -- version     :: Bool,
  seq         :: Bool,
  stream      :: Bool,
  slurp       :: Bool,
  rawInput    :: Bool,
  nullInput   :: Bool,
  compactOut  :: Bool,
  indent      :: Indent,
  colorOut    :: Color,
  asciiOut    :: Bool,
  unbuffered  :: Bool,
  sortKeys    :: Bool,
  rawOut      :: Bool,
  joinOut     :: Bool,
  filterInput :: FilterInput,
  moduleDir   :: Maybe FilePath,
  exitStatus  :: Bool,
  -- TODO(tobi)
  -- args :: [(Text, Text)],
  -- jsonArgs :: [(Text, ByteString)],
  -- slurpfile :: ArgFile,
  -- rawfile :: ArgFile,
  -- posArgs :: [Text],
  -- posJsonArgs :: [ByteString],
  runTests :: Maybe FilePath
} deriving (Eq, Show)

getOptions :: IO Options
getOptions = customExecParser helpPrefs optionsParser

helpPrefs :: ParserPrefs
helpPrefs = prefs
  (  showHelpOnEmpty
  <> columns 110
  )

optionsParser :: ParserInfo Options
optionsParser = info (options <**> helper <**> renderVersion)
  ( fullDesc
  <> progDesc "TODO"
  <> header ("jqhs - haskell implementation of the jq commandline JSON processor [version " <> jqhsVersion <> "]")
  <> failureCode 1
  )

options :: Parser Options
options = Options
      -- <$> versionArg
      <$> seqArg
      <*> streamArg
      <*> slurpArg
      <*> rawInputArg
      <*> nullInputArg
      <*> compactOutArg
      <*> indentArg
      <*> colorOutArg
      <*> asciiOutArg
      <*> unbufferedArg
      <*> sortKeysArg
      <*> rawOutArg
      <*> joinOutArg
      <*> filterInputArg
      <*> moduleDirArg
      <*> exitStatusArg
      -- TODO(tobi)
      -- <*> argsArg
      -- <*> jsonArgsArg
      -- <*> slurpfileArg
      -- <*> rawfileArg
      -- <*> posArgsArg
      -- <*> posJsonArgsArg
      <*> runTestsArg

renderVersion :: Parser (a -> a)
renderVersion = infoOption ("jqhs-" <> jqhsVersion)
  (  long "version"
  <> help "Output the jq version and exit with zero."
  )

seqArg :: Parser Bool
seqArg = switch
  (  long "seq"
  <> help "\
      \Use the application/json-seq MIME type scheme for separating JSON texts in jq\'s input and\
      \ output. This means that an ASCII RS (record separator) character is printed before each value on\
      \ output  and  an ASCII LF (line feed) is printed after every output. Input JSON texts that\
      \ fail to parse are ignored (but warned about), discarding all subsequent input until the next RS.\
      \ This mode also parses the output of jq without the --seq option.\
      \"
  )

streamArg :: Parser Bool
streamArg = switch
  (  long "stream"
  <> help "\
      \Parse the input in streaming fashion, outputing arrays of path and leaf values (scalars and\
      \ empty arrays or empty objects). For example, \"a\" becomes [[],\"a\"], and [[],\"a\",[\"b\"]]\
      \ becomes [[0],[]], [[1],\"a\"], and [[1,0],\"b\"].\n\
      \\n\
      \This is useful for processing very large inputs. Use this in conjunction with filtering and the\
      \ reduce and foreach syntax to reduce large inputs incrementally.\
      \"
  )

slurpArg :: Parser Bool
slurpArg = switch
  (  long "slurp"
  <> short 's'
  <> help "\
      \Instead of running the filter for each JSON object in the input, read the entire input stream\
      \ into a large array and run the filter just once.\
      \"
  )

rawInputArg :: Parser Bool
rawInputArg = switch
  (  long "raw-input"
  <> short 'R'
  <> help "\
      \Don\'t  parse  the  input as JSON. Instead, each line of text is passed to the filter as a string.\
      \ If combined with --slurp, then the entire input is passed to the filter as a single long string.\
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

compactOutArg :: Parser Bool
compactOutArg = switch
  (  long "compact-output"
  <> short 'c'
  <> help "\
      \By default, jq pretty-prints JSON output. Using this option will result in more compact output\
      \ by instead putting each JSON object on a single line.\
      \"
  )

indentArg :: Parser Indent
indentArg =
  flag' Tab
    (  long "tab"
    <> help "Use a tab for each indentation level instead of two spaces."
    )
  <|>
  Space <$> option auto
    (  long "indent"
    <> help "Use the given number of spaces (no more than 8) for indentation."
    <> metavar "n"
    )
  <|>
  pure (Space 4)

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
        \ https://www2.ccs.neu.edu/research/gpc/VonaUtils/vona/terminal/vtansi.htm.\
        \"
    )
  <|>
  flag' CDisabled
    (  long "monochrome-output"
    <> short 'M'
    <> help "Disable colored JSON output, even when writing to a terminal."
    )
  <|>
  pure CDefault

asciiOutArg :: Parser Bool
asciiOutArg = switch
  (  long "ascii-output"
  <> short 'a'
  <> help "\
        \jq usually outputs non-ASCII Unicode codepoints as UTF-8, even if the input specified them as\
        \ escape sequences (like \"\\u03bc\"). Using this option, you can force jq to produce pure  ASCII\
        \ output with every non-ASCII character replaced with the equivalent escape sequence.\
        \"
  )

unbufferedArg :: Parser Bool
unbufferedArg = switch
  (  long "unbuffered"
  <> help "\
      \Flush the output after each JSON object is printed (useful if you\'re piping a slow data source\
      \ into jq and piping jq\'s output elsewhere).\
      \"
  )

sortKeysArg :: Parser Bool
sortKeysArg = switch
  (  long "sort-keys"
  <> short 's'
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

moduleDirArg :: Parser (Maybe FilePath)
moduleDirArg = optional $ strOption
  (  short 'L'
  <> help "\
    \Prepend directory to the search list for modules. If this option is used then no builtin search\
    \ list is used. See the section on modules below.\
    \"
  <> metavar "directory"
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

runTestsArg :: Parser (Maybe FilePath)
runTestsArg = optional $ strOption
  (  long "run-tests"
  <> help "\
    \Runs the tests in the given file or standard input. This must be the last option given and does\
    \ not honor all preceding options. The input consists of comment  lines,  empty  lines,  and\
    \ program  lines  followed by one input line, as many lines of output as are expected (one per\
    \ output), and a terminating empty line. Compilation failure tests start with a line containing\
    \ only \"%%FAIL\", then a line containing the program to compile, then a line containing an error\
    \ message to compare to the actual.\n\
    \\n\
    \Another way to set the exit status is with the halt_error builtin function.\
    \"
  <> metavar "testpath"
  )