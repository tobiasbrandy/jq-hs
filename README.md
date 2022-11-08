# jq-hs
## Pure Haskell implementation of JQ JSON processor

The objective of this project is to emulate the behaviour of jq command (version 1.6) as closelly as possible, and also provide a library that can be embedded on other Haskell projects.

The main focus is not on performance, at least not until all jq features are implemented.

This codebase was born as a pet project in order to learn Haskell, so the code quality may reflect that.

For more information about jq and it's manual, please visit the [official website](https://stedolan.github.io/jq) where you can find the updated [manual](https://stedolan.github.io/jq/manual/).

### Project structure

The project uses the [Stack](https://docs.haskellstack.org/en/stable/) build system. All project configuration is declared on `stack.yaml` and `package.yaml`.

As it is common with this build system, the project is divided in 3 parts.
  1. The package called `jq-hs`, which may be imported onto any other existing Haskell project. This package is completely pure, and doesn't deal with any IO. Everything under `src/` corresponds to this package.
  2. The tests for `jq-hs` are all under `test/`, and may be executed running `stack test`.
  3. The executable `jqhs`, which creates the CLI program that tries to emulate the `jq` utility. This package deals with exercising the `jq-hs` library, dealing with IO and offering a CLI interface with multiple options. This package may be installed running `stack install`.

### Installation

The project has 2 external dependencies that must be installed separatelly.

The first one is [GMP](https://gmplib.org/), which is a dependency of GHC itself for the `Integer` type implementation, among other similar types. This requirement may be satisfied with other options, such as [ghc-bignum](https://hackage.haskell.org/package/ghc-bignum) or [integer-gmp](https://hackage.haskell.org/package/integer-gmp), but some project configuration will probably need to be modified.
One option on Ubuntu 20.04 is the GMP development version, which can be installed running `sudo apt install libgmp-dev`.

The second one is the development version of the regular expressions library [Oniguruma](https://github.com/kkos/oniguruma), which is used to implement the regular expressions builtins (such as `test` and `match`). The original jq also depends on Oniguruma to this end.
On Ubuntu 20.04, this dependency can be installed running `sudo apt install libonig-dev`.

After installing all the required external dependencies, we just need to run `stack install` to download, build and install all the remaining dependencies, and then build and install in our system the `jqhs` executable.

### Dependencies
Importance was given to keep the number of dependencies low and within the core Haskell libraries.

#### Core
  - base
  - [bytestring](https://hackage.haskell.org/package/bytestring)
  - [array](https://hackage.haskell.org/package/array)
  - [text](https://hackage.haskell.org/package/text)
  - [scientific](https://hackage.haskell.org/package/scientific)
  - [hashable](https://hackage.haskell.org/package/hashable)
  - [containers](https://hackage.haskell.org/package/containers)
  - [unordered-containers](https://hackage.haskell.org/package/unordered-containers)
  - [file-embed](https://hackage.haskell.org/package/file-embed)
  - [base64](https://hackage.haskell.org/package/base64)

#### Build Tools
  - [happy](https://hackage.haskell.org/package/happy)
  - [alex](https://hackage.haskell.org/package/alex)

#### External
  - [GMP](https://gmplib.org/)
  - [Oniguruma](https://github.com/kkos/oniguruma)

### Missing Features
Not all features are currently implemented, as they were deemed too complex or not central enough in the jq toolkit. In the future they may be added. This are:
  - Modules and Imports: This feature was introduced in version 1.6, which allowed to search external modules with function declarations and load then as needed with new import keywords. This is, of course, quite complex. The builtins `modulemeta/0` and `get_search_list/0`, which only make sense in the context of this feature, are also missing.
  - Destructuring: In order to keep the parsing as simple as possible, and because the functionality this feature provides is essentialy syntax sugar, the implementation of destructuring was pushed into the future. This includes the `?//` operator, which only makes sense in the context of this feature.
  - Compile time existance checks for Variables, Functions and Labels: jq will fail on compilation if an undeclared variable, function or label is used. Currently, jqhs doesn't provide this usefull feature, only failing at runtime if the undeclared identifier is called.
  - Helpfull compilation and runtime error messages: jq makes an effort to provide error messages that help the user track the error. This feature is partially missing on runtime errors, and completely missing on compilation errors. Until more features are implemented, this is not the focus of the current development.

### Missing Builtins
Currently `197/216` builtin functions are implemented. The missing functions are:
  - Date Functions. This functions are a thin wrapper of the POSIX time functions, so access to the underlying C functions would be the direct way to implement them, but that would require to deal with multiple system dependent behaviour. Reimplementing them in Haskell is probably the more sane approach, but the lack of a complete and standard time library in Haskell makes it more complex than it first appears. The functions are:
    - `strptime/1`
    - `strftime/1`
    - `strflocaltime/1`
    - `mktime/0`
    - `gmtime/0`
    - `localtime/0`
    - `now/0`
    - `fromdateiso8601`
    - `todateiso8601`
    - `fromdate`
    - `todate`

  - IO Functions. Currently there are 2 types of missing IO functions. The first type are the ones that consume the input from inside a program. Their implementation is currently in development, but it was delayed due to conflicts with how function recursion is currently implemented. The second kind are the ones that access file information of the current input. Since, currently, jqhs treats any input as a bytestream without recording any other input information, it is not possible to implement them. Furthermore, it is not clear that the inclusion of this functions justifies the architectural change, with all it's drawbacks. The functions are:
    - `input/0`
    - `inputs/0`
    - `input_filename/0`
    - `input_line_number/0`

  - Environment Functions. This functions get information about the context of the jq program execution. This functions are poorly documented and unstable. They are not implemented by choice. This functions are:
    - `get_prog_origin/0`
    - `get_jq_origin/0`

  - Modules: Since the modules feature is not implemented, builtins associate with this feature are also not included. This functions are:
    - `modulemeta/0`
    - `get_search_list/0`

### Missing executable options
Not all jq executable options are yet implemented. The missing ones are:
  - `--seq`
  - `--stream`
  - `--raw-input/-R`
  - `--ascii-output`/`-a`
  - `--unbuffered`
  - `-Ldirectory`/`-L directory`: Only makes sense with Modules feature.
  - `--arg <name> <value>`
  - `--argjson <name> <JSON-text>`
  - `--slurpfile variable-name filename`
  - `--rawfile variable-name filename`
  - `--argfile variable-name filename`
  - `--args`
  - `--jsonargs`
  - `--run-tests <filename>`

### Known Bugs
The following snippets are known to yield invalid results or fail on execution:
  - `echo '{}' | jqhs 'path(reduce (0,1) as $n (.a;.a))'`: returns `["a","a","a"]` instead of `["a"]`
  - `echo 'null' | jqhs 'path(foreach (0,1,2,3) as $n (.[0];.[0];.[$n]))'`: returns `[0,0,0] [0,0,0,1] [0,0,0,0,2] [0,0,0,0,0,3]` instead of `[0,0,0] [0,0,1] [0,0,2] [0,0,3]`
  - `echo '1' | jqhs '[repeat(.*2, error)?],[repeat(.*2, error)?]'`: doesn't finish
