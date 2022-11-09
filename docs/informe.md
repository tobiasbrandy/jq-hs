# Informe Proyecto Final Funcional - jq-hs

## Introduccion
El objetivo de este trabajo es el de emular el comportamiento del programa de línea de comandos jq (version 1.6) mediante una aplicacion codificada purament en Haskell, y tambien proveer una libreria que pueda ser embebida en otros proyectos Haskell. El proyecto busca ser portable, y de bajas dependencias.

El principal foco del trabajo no es la performance, sino la replicacion fiel de la mayor parte de los comportamientos de jq. Por supuesto, la misma es valorada.

Jq es una herramienta de procesamiento de JSONs que los permite filtrar, mapear y transformar. Busca emular la facilidad de uso de otras herramientas de linea de comando como sed, awk o grep, pero enfocado especificamente en el formato JSON.

Este programa recibe un stream de JSONs, los pasa por una serie de filtros declarados mediante el lenguaje propio de jq, y luego imprime los JSON resultantes. tengamos en cuenta que la cantidad de inputs y de outputs de un filtro no tienen relacion, pues cada filtro puede decidir emitir 0, 1 o multiples respuestas, que luego seran la entrada del proximo filtro.

Un ejemplo de un programa jq es `echo '{"foo": "bar"}' | jq '.foo'`. El mismo recibe un JSON por entrada estandar, projecta la propiedad `foo` del mismo, e imprime el resultado `"bar"`. Otro ejemplo es `echo '[1,2,3,4]' | jq '.[] += 1'`. El mismo le suma 1 a cada elemento del array de entrada y luego imprime el resultado, es decir, `[2,3,4,5]`.

Para ser mas concretos, el objetivo del trabajo es generar un ejecutable `jqhs`, que se comporte de la misma manera que jq. Esto quiere decir que si en cualquiera de los ejemplos de arriba remplazaramos jq por jqhs, el resultado debiera ser el mismo.

Para mas información acerca de jq, por favor visitar la [página oficial](https://stedolan.github.io/jq) donde se pueden encontrar un tutorial y más ejemplos, junto con el [manual](https://stedolan.github.io/jq/manual/) actualizado donde se detallan la mayor parte de las características. Además, jq posee todo su [código de forma abierta](https://github.com/stedolan/jq), el cual fue referenciado durante le desarrollo de la aplicación múltiples veces.

## Arquitectura general
Este proyecto utiliza [Stack](https://docs.haskellstack.org/en/stable/) como su sistema de build. Toda la configuración del mismo se encuentra en los archivos `stack.yaml` y `package.yaml`.

### Paquetes
Como es común con este sistema de build, el proyecto esta dividido en 3 paquetes.

El primero y central es la libreria `jq-hs`, la cual incluye la lógica core de la aplicación, que puede ser embebida en cualquier otro proyecto Haskell como una dependencia más. El mismo es completamente puro, y no se encarga de nada relacionado con IO.
Las principales tareas del modulo son: parsear tanto los input JSON como el programa jq a ejecutar, implementar la librería estandar del lenguaje jq, ejecutar por cada input todos los filtros del programa jq y ofrecer opciones de pretty printing de los JSON resultantes. Todos los módulos que se encargan de esto se encuentran dentro de la carpeta `src/`.

El segundo paquete es el encargado de testear la libreria `jq-hs` antes descripta. La estrategia utilizada son tests de punta a punta declarados en archivos de extensión `.jq.test`.
Cada test esta separado por una linea vacía. Un test esta compuesto por una primera linea que indica el programa jq a ejecutar, la segunda cada input JSON separado por espacio que se debe ejecutar, y luego el resto de las lineas es cada output en orden que el programa debe generar. Las lineas que comienzan con `#` son comentarios y se ignoran.
Actualmente se cuentan con `531` tests ejecutados exitosamente, todos extraidos de la suite de tests oficial de jq, lo cual da cierta garantía de similitud de comportamiento.
Todos los tests se encuentran dentro de `test/`, siendo el punto de entrada `Spec.hs`, y pueden ser ejecutados mediante le comando `stack test`.

El tercer y último paquete es el ejecutable `jqhs`, el cual se encarga de ejercitar toda la funcionalidad ofracida por la librería `jq-hs`, implementar toda la lógica de IO y ofrecer una interfaz de línea de comandos con múltiples opciones.
Este paquete se encuentra dentro de `app/`, y esta compuesta únicamente por `Main.hs`, el punto de entrada de la aplicación, y `Options.hs`, que se encarga de la declaracion y parseo de las opciones de línea de comando. Podemos instalar el programa ´jqhs´ en nuestro sistema mediante el comando `stack install`.

### Dependencias
Durante el desarrollo se intentó mantener el número de depencias bajo, y intentando que sean únicamente dependencias chicas (bajos tiempos de compilación) y confiables (estándars en el entorno de desarrollo haskell).

Podemos distinguir entre 4 tipos de dependencias:
  - Core: Dependencias tradicionales programadas en Haskell y utilizadas en tiempo de compilación y ejecución en la aplicación.
  - Externas: Dependencias externas necesarias únicamente a partir de la linkedición del programa, no necesariamente programadas en Haskell. Para la correcta compilación de nuestro programa vamos a necesitar que ya esten instaladas en nuestro sistema de manera externa.
  - Build: Dependencias utilizadas únicamente en el proceso de compilación para generar la librería.
  - Ejecutable: Dependencias únicamente necesarias por el ejecutable, y no la librería.
  - Test: Dependencias únicamente utilizadas por el módulo de test.

#### Dependencias Core
  - base: Libreria estándar de Haskell. En todos los archivos se hace uso del Prelude estándar.
  - [bytestring](https://hackage.haskell.org/package/bytestring). Ofrece la estructura de datos `ByteString` que permite representar un stream de bytes de manera eficiente. Es utilizado para codificar informacion de entrada/salida de la aplicacion. Siempre es considerado como datos "inseguros", que primero deben ser parseados para poder utilizar. Durante toda la aplicación es utilizada con una codificación UTF-8. Toda información que ingresa a la aplicación debe utilizar este esquema de codificación.
  - [array](https://hackage.haskell.org/package/array). Esta dependencia no es utilizada directamente, sino por código generado por las dependencias de Buils, en particular por `alex`.
  - [text](https://hackage.haskell.org/package/text). Ofrece la estructura de datos `Text`, que permite representar una cadena de caracteres Unicode. Este es un reemplazo directo a `String`, el cual es mas eficiente, ofrece mejores métodos de manipulación y simplifica el correcto manejo de caracteres Unicode, requerimiento de la aplicación. Esta sería la contracara de `ByteString`, es decir, si el este representa texto externo e inseguro, `Text` representa texto de nuestra aplicación que podemos utilizar con confianza. Se intenta utilizar `String` lo menos posible.
  - [scientific](https://hackage.haskell.org/package/scientific). Ofrece la estructura de datos numérica `Scientific`, que representa numeros en notación científica, tanto enteros como decimales. Se elige esta representación para los numeros de un JSON, pues la misma puede almacenarlos con mínimo parsing, siendo seguro aún para números inseguros. Además, provee una manera común de representar los números enteros y decimales. Un detalle interesante de esta representación es que si bien es seguro almacenarla, para operar es necesario coercionarlo a otro tipo de dato (en la aplicación se eligio Integer y Double), para luego volver a esta representación original.
  - [containers](https://hackage.haskell.org/package/containers). De esta dependencia se utiliza principalmente la estructura de datos `Sequence`, la cual nos deja representar la información de manera mas compacta que las listas tradicionales, tiene orden logaritmo para el acceso random y tiempo lineal desde cualquiera de sus extremos, sin perder las propiedades lazy de las listas. Se elige esta representación para los arrays de un JSON, pues es útil esta flexibilidad agregada. Otra opción hubiese sido `Vector`, que si bien es mas eficiente, pierde las propiedades lazy. Una contra grande de `Sequence` es que soporta un maximo tamaño de 64 bits, mientras que otras opciones no tienen esta limitación.
  - [unordered-containers](https://hackage.haskell.org/package/unordered-containers). De esta dependencia se utiliza principalmente la estructura de datos `HashMap`. Los mapas tradicionalmente utilizados en Haskell son este o `Map` de `containers`. Las principales ventajas del `HashMap` es que no requiere que las claves posean un orden (aunque si que sean hasheables), es más performante y no posee un limite máximo (donde la otra opcion tiene un máximo de 64 bits).
  - [hashable](https://hackage.haskell.org/package/hashable). Se utiliza para poder utilizar tipos de datos propios como claves de un `HashMap`.
  - [base64](https://hackage.haskell.org/package/base64). Esta dependencia ofrece una implementación performante escrita en Haskell de los algoritmos de codificación y decodificación de base64.
  - [file-embed](https://hackage.haskell.org/package/file-embed). Esta dependencia utiliza TemplateHaskell para embeber recursos textuales en el binario de la aplicacion, para poder ser accedidos directamente como un `ByteString`. Esta técnica es performante y simplifica el manejo de recursos estáticos, sin tener que manejar IO. El mismo se usa en la librería para embeber las funciones de la librería estandar de jq implementadas en el lenguaje jq, y en las suites de tests para embeber los archivos `.jq.test` que declaran los tests.

#### Dependencias Externas
  - [GMP](https://gmplib.org/). Esta dependencia aritmética es requerida por GHC para implementar la estructura de datos `Integer`, junto a otros tipos similares. Es posible que existan otras opciones para suplir este requerimiento, como por ejemplo [ghc-bignum](https://hackage.haskell.org/package/ghc-bignum) o [integer-gmp](https://hackage.haskell.org/package/integer-gmp), pero seguramente requieran modificar la configuración del proyecto.
  - [Oniguruma - Dev](https://github.com/kkos/oniguruma). Esta es una librería de expresiones regulares que es utilizada para implementar las funciones de expresiones regulares de la librería estandar de jq (por ejemplo `test` y `match`). Jq también utiliza esta dependencia para este fin.
  
#### Dependencias de Build
  - [alex](https://hackage.haskell.org/package/alex). Esta dependencia ofrece el lexer que fue utilizado para el parsing de JSON y de los programas jq. Mediante un DSL, declaramos las reglas (similares a regex) por las cuales matcheamos una cadena de caracteres a un token específico, que luego serán utilizados por el parser para generar el AST. El archivo de reglas que posee extensión `.x` es (en tiempo de compilacion) pre-procesado por `alex` para generar un archivo Haskell válido.
  - [happy](https://hackage.haskell.org/package/happy). Esta dependencia ofrece el parser que fue utilizado para el parsing de JSON y de los programas jq. Mediante un DSL, declaramos reglas (similares a una gramática) por las cuales declaramos la sintáxis del lenguaje basada en los tokens generados por el lexer y especificamos las reglas por las cuales iremos formando el AST. El archivo de reglas que posee extensión `.y` es (en tiempo de compilación) pre-procesado por `happy` para generar un archivo haskell válido.

#### Dependencias del Ejecutable
  - [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative). Esta es la dependencia utilizada para leer, validar y parsear la interfaz de línea de comandos ofrecida por la aplicación. Posee una api aplicativa declarativa sencilla de utilizar.

#### Dependencias de Testing
- [HUnit](https://hackage.haskell.org/package/HUnit). Este framework de testing se inspira en JUnit para la declaracion de suits de tests, y de asserts durante los mismos.

## Módulos
En esta sección la idea es discutir brevemente la función de cada módulo dentro de la aplicación.

### Ejecutable
Al paquete del ejecutable se buscó que posea la menor cantidad de lógica, delegandole únicamente las tareas relacionadas al IO. Es por esto que actualmente únicamente posee dos módulos relativamente cortos y sencillos.

#### Main.hs
Este es el punto de entrada de la aplicacíon, y el que se encarga de todo lo relacionado a IO. Se encarga de encadenar las funcionalidades de la librería en el siguiente order:
  1. Parsea, valida y obtiene las opciones por línea de comandos.
  2. Parsea el programa jq, es decir, el filtro a ejecutar.
  3. Parsea cada uno de los inputs del programa.
  4. Ejecuta el filtro por cada uno de los inputs, obteniendo los outputs.
  5. Imprime cada output a pantalla.
  6. Termina ejecución con el código adecuado.

Tengase en cuenta que es importante que los pasos 3 a 5 se hacen input por input, es decir, no se espera a parsear todos los inputs antes de pasar al siguiente paso. Si el tercer input lanza un error de parseo, los primeros 2 ya fueron parseados, ejecutados e impresos.

#### Options.hs
Este es el módulo que se encarga de definir, leer, parsear, validar y retornar todas las opciones del programa que pueden ser ingresadas por línea de comandos. El valor de cada opcion es almacenada en la estructura Options, la cual es retornada para ser utilizada por Main.hs.

### Data.Parser
En este paquete vamos a encontrar todas las funcionalidades para construir un parser con alex y happy (dentro del paquete `Data.Parser.Build`), y luego ejecutarlo (dentro del módulo `Data.Parser.Parse.hs`).
Las funciones de construcción son genéricas para cualquier parser de este estilo, luego cada parser tendrá que únicamente proveer los tokens a utilizar, el archivo alex (`.x`) y el archivo happy (`.y`). El tipo de dato principal de este módulo es la mónada `Parser token a`, similar a la mónada `State a`.
Cabe destacar que Alex y Happy serán siempre usados en su modo monádico sin ningún tipo de wrapper, es decir, definiendo todos la mayor cantidad de tipos y funcionamientos que estas utilidades permiten. Se hace esto para obtener más flexibilidad a la hora de utilizar las herramientas, ademas de no leakear detalles de implementación por fuera del paquete.
Se decidió utilizar a Alex y Happy para el parsing, pues implementan parsers LALR(1), los cuales dan mucha flexibilidad (casi tanto como LR(1)), pudiendo resolver conflictos shifteando y hacer recursión a izquierda, de manera performante y sin backtracking (en principio). Otras opciones, como los parser combinators, resultaban muy restrictivas (pues son LL(1) o LL(inf)) y no garantizaban performance.

#### Data.Parser.Parse.hs
En este módulo se encuentran las funciones para ejecutar un parser, ya sea exactamente para obtener un valor (`parseOne`), como para obtener todos los disponibles (`parseAll`). Para esto se necesita un parser a ejecutar, y el estado inicial del mismo.

#### Data.Parser.Build.Parser.hs
Este es el módulo principal del paquete de build, donde se define la mónada `Parse token a` junto con su estado asociado `ParserState token`, y el tipo de resultado `ParserResult token result`.

`Parse` es una mónada similar a `State a`, que modela estado global del parser. En este estado vamos a encontrar:
  - `p_pos`: La fila y columna actual del parser en el archivo que se esta parseando. Esta información es útil a la hora de informar errores.
  - `p_bpos`: La cantidad de bytes leidos hasta ahora.
  - `p_input`: El input restante a consumir.
  - `p_pushedToks`: Un stack de tokens empujados manualmente a ser procesados antes de continuar consumiendo el input. Muy útil para agregar tokens intermedios artificiales.
  - `p_tokBuilder`: Es posible que un token requiera de varias corridas del lexer para ser generado en su completitud. Aqui se puede almacenar el estado intermedio del token, sin ser consumido por el parser. Un ejemplo es un string que en el medio tiene caracteres que debemos des-escapar.
  - `p_codes`: Un stack de startcodes que utiliza el lexer. Sirve para iniciar un nuevo estado, sin pisar los estados anteriores. Por ejemplo, con esta funcionalidad podemos saber la profundidad de apertura de paréntesis, y si falto cerrar alguno.

`ParserResult` es similar a la mónada Either, teniendo un estado exitoso y otro erróneo.

El resto de los métodos son de convenciencia para acceder a la mónada `Parser`.

#### Data.Parser.Build.Lexing.hs
En este módulo se incluyen todas las funciones de utilidad genéricas que puede necesitar un lexer Alex. Tengamos en cuenta que lo ideal es tener la menor cantidad de código Haskell posible en los archivos `.x`.
Entre las funcionalidades más importantes se encuentran la generación de un error causado por el lexer, construcción de tokens, el parseo de números y des-escapado de strings y utilidades para el manejo de la mónada `Parser` (startcodes, token builders, etc).
Tambien se define el tipo `LexAction token`, que es el tipo que tendran los snippets de código de cada regla del lexer Alex que se usarán para, a partir del input matcheado, generar el token correspndiente. Se define el tipo para comodidad.

#### Data.Parser.Build.AlexIntegration.hs
En este módulo se definen los tipo de datos y funciones que la interfaz de Alex define que necesita para poder funcionar.

#### Data.Parser.Build.Parsing.hs
En este módulo se declaran todas las funciones de utilidad genéricas que puede necesitar un parser Happy. Actualmente, esto significa únicamente una función que genera una respuesta de error causado por el parser.

### Data.Json
En este paquete vamos a encontrar todo lo referido al tipo de dato `Json`, el cual vamos a utilizar para representar un archivo JSON. En este vamos a encontrar la definición, parseo y pretty printing del mismo.

#### Data.Json.hs
En este módulo se define el tipo de dato `Json`, el cual representará a un JSON genérico a lo largo de toda la aplicación. Cabe destacar que como el tipo de dato `Scientific` no puede representar el tipo de dato NaN y la aplicación requiere manejar este valor, se debe wrappear el tipo con un `JsonNum`, mónada similar a `Maybe`, pero con semántica de número.
El diseño de este tipo de dato esta inspirado en el de la popular librería `aeson`.

#### Data.Json.Parsing.Tokens.hs
En este módulo simplemente se declaran los tokens a utilizar durante el parsing de `Json`, de tipo `JsonToken`.

#### Data.Json.Parsing.Lexer.x
Este módulo es un lexer Alex que busca generar los tokens para parsear un `Json`. Se puede destacar que sólo hace falta declarar la función principal del lexing: `lexer`. Esta debería ser muy similar para todos los lexers, pero no es posible generalizarla, pues Alex realiza manipulaciones textuales. De todas maneras, el snippet de codigo que es necesario de declarar es muy sencillo y reducido.
La mayoría de las definiciones de los tokens vienen de la [especificación oficial de JSON](https://www.json.org/json-en.html) y de `docs/json-grammar.md`.
La única particularidad que podemos encontrar en este lexer es que, como va a ser necesario parsear múltiples JSON del mismo input, no fallamos inmediatamente, sino que generamos un token de error, para que el parser pueda decidir cuando generar el error, una vez que haya tenido tiempo de generar su resultado, en caso de que fuera necesario.

#### Data.Json.Parsing.Parser.y
Este módulo es un parser Happy que busca declarar las reglas de sintaxis de un JSON para, a partir de los tokens generados por el lexer, armar los `Json`. La mayoría de las reglas fueron tomadas de la [especificación oficial de JSON](https://www.json.org/json-en.html) y de `docs/json-grammar.md`.
Podemos notar que este parser tiene la configuracion `%partial`, esto es porque esperamos poder parsear múltiples `Json` de un mismo input, sin un separados entre ellos. Una vez que terminamos de parsear uno, empezamos a parsear el siguiente.
Una particularidad de Happy es que recien va a terminar de parsear cuando se genere un error estando en el root del arbol de parseo, esto causa de que siempre se lea un token demás. Para mitigar esto, en caso que se haya parseado exitosamente, pero aún no se haya alcanzado el End Of File, nos vamos a guardar el próximo token, para no perderlo.
Otra particularidad es que, como el lexer delega la responsabilidad de generar el error de lexing al parser para que se pueda parsear múltiples JSON, debemos wrappear la función de error del parser, y además crear una regla `lexError`, que no permita que la misma pase sin generar el error.
Por último, podemos ver que se decidió que el caso de que no haya un JSON para parsear no se considere como un error, sino que simplente la aplicación no ejecute nada ante la ausencia de input.

#### Data.Json.Encode.hs
Este módulo ofrece un metodo principal de deserialización de `Json` a `ByteString` llamado `jsonEncode`, que acepta una amplia gama de configuración de formato en la forma del tipo `Format`. El mismo esta compuesto por:
  - `fmtIndent`: Cantidad de indentación a utilizar. Las opciones son: compacto, es decir sin ningún tipo de white space, que se representa como 0 espacios; con una cantidad n > 0 de espacios; o con tabs.
  - `fmtCompare`: Función de ordenamiento para los items de un objeto, según su clave.
  - `fmtColors`: Como colorear las distintas partes del output, o si no colorear nada (`Nothing`). Para colorear se utilizan los ANSI escape sequences soportados por cualquier terminal textual.
  - `fmtRawStr`: En caso de que se active esta opción y se encodee un Json String, la misma no será escapada, ni se le agregaran las comillas.
  - `fmtTrailingNewline`: En caso de activarse, se agregará un `\n` como último caracter.

El diseño de este módulo esta inspirado en la librería `aeson-pretty`, que busca algo similar para el tipo de dato Json de la librería `aeson`.

### Data.Filter
En este paquete vamos a encontrar todo lo relacionado al tipo `Filter`, el tipo de dato que representa un AST de un programa jq.
En este vamos a encontrar el parseo de un programa jq, la implemnetación de las reglas de ejecución del mismo, y la implementación de la librería estandar de jq, tanto en código Haskell, como en código jq que se aprovecha de otras funcionalidades ya implementadas.

#### Data.Filter.hs
En este módulo simplemente se define el tipo de dato `Filter`, el cual funciona de AST de un programa jq.

#### Data.Filter.Parsing.Tokens.hs
En este módulo simplemente se declaran los tokens a utilizar durante el parsing de `Filter`, de tipo `FilterToken`.

#### Data.Filter.Parsing.Lexer.x
Este módulo es un lexer Alex que busca generar los tokens para parsear un `Filter`. La especificación de los mismos fue tomado del repo oficial de jq (que utiliza `flex`), y de `decs/jq-grammar.md`. Se puede destacar que para el procesamiento de strings y su escapado se debió utilizar una configuración más compleja, que involucró tokens parciales. Lo mismo sucedió con la interpolación de strings, sumado a la necesidad de utilizar multiples startcodes stackeados, para mantener contexto.

#### Data.Filter.Parsing.Parser.y
Este módulo es un parser Happy que busca declarar las reglas de sintaxis de un programa jq para, a partir de los tokens generados por el lexer, armar los `Filter`. La mayoría de las reglas fueron tomadas del repo oficial de jq y de `docs/jq-grammar.md`.
Se puede ver que esta vez se necesitó de un uso intensivo de reglas recursivas a izquierda y de resolución de conflictos shift-reduce mediante precedencia y sentencias `%shift`. Podemos ver también que este parser no está diseñado para ser llamado múltiples veces, pues en un mismo input sólo debería haber un único programa jq.b

#### Data.Filter.Run.hs
Este módulo se encarga únicamente de exportar una api amigable para consumo externo, haciendo de facade del modulo interno `Data.Filter.Internal.Run.hs`. El mismo da una versión reducida de `FilterRet`, eliminando constructores que sólo tienen sentido a nivel implementación. también se encarga de re-exportar únicamente los métodos que tienen sentido que un usuario tenga acceso, el resto los mantiene sólo como internos.

#### Data.Filter.Internal.Result.hs
Este módulo de utilidad define el tipo y el comportamiento de `FilterRet`. Esta simple abtracción representa todas los posibles resultados que puede retornar un filtro. Luego, como un filtro puede retornar multiples (o ningún resultado), se define el alias de tipo `FilterResult`, que es una lista de `FilterRet`.
Los posibles valores de retorno son:
  - `Ok a`: Respuesta válida con un resultado, en general un `Json`. Esta es la respuesta típica.
  - `Err Text`: Respuesta que representa un error, con un mensaje que indica cual fue, y corta ejecución, es decir, se debe descartar el resto de las respuestas. Similar a `Left` de `Either`.
  - `Break Text`: Respuesta que representa un corte de ejecucion desde el punto que se encuentra hasta la ultima label con el nombre que contiene. Este valor de retorno es interno,nunca debería llegar hasta el usuario, pues para todo break debe existir una label.
  - `Halt Int (Maybe Json)`: Respuesta que representa la terminación de la ejecución del programa, con un Int que es el código de retorno de la aplicación y, opcionalmente, un valor a imprimir por stderr.
  - `Stderr Json`: Valor de retorno que contiene un valor a imprimir a stderr, similar a la funcion `trace` de base. Nunca se genera sólo, siempre luego de este tipo de valor de retorno hay más.
  - `Debug Json`: Muy parecido a `Stderr`, pero con semántica de debug, que lleva a que sea utilizado por el usuario de manera distinta.

Como podemos ver, la semántica de los valores de `FilterRet` indican como deben ser procesado los siguientes `FilterRet` en una lista (un `FilterResult`), cortando o no la ejecución de los demas valores sin perder los valores de los ya procesados, sin importar si tienen un valor o no. No se consiguió modelar este comportamiento a traves de la interfaz `Traversable` (la cuál parece ser la que más se aproxima, gracias a su método sequence y similares). Es por eso que reimplementan métodos estandar como `fold`, `map`, `foldM`, `mapM`, `concatMapM`, entre otros, pero con un `Ret` al final, lo que indica que respeta esta semántica que acabo de describir. Se podrá ver que a la hora de procesar los `Filter`, se utilizará el tipo de dato `FilterResult` como valor de retorno, junto con estas funciones que permiten procesarlos correctamente.
De todas maneras, `FilterRet` implementa la interfaz mónada, pero con una semántica similar a la de `Either`, de retornar una lista de `Ok`, o el primer valor que no fue `Ok`, es decir, de circuit breaker. Hay que ser concientes cuendo utilizamos esta funcionalidad.
La funcion `applyByInterrupt` le otorga la semántica a los distintos valores de `FilterRet` acerca de cuales deben interrumpir el flujo de ejecución y cuales no. El resto de las funciones utilizan esta para diferenciar, manteniendo el contrato.

#### Data.Filter.Internal.Run.hs
Este módulo es el core del paquete. Aquí, en la función `runFilter`, se define el comportamiento de cada nodo del AST que es `Filter`, ejecutandolo contra un `Json` y retornando `FilterRun (FilterResult (Json, Maybe PathExp))`. 
`FilterResult` es definido en `Data.Filter.Internal.Result.hs` y ya fue definido.
`FilterRun` es otra mónada State, que tiene asociada el estado `FilterRunState` compuesta por:
  - `fr_path_exp`: Indica si se debe ir generando la path expression (`PathExp`) junto con el resultado de las operaciones. Solo las operaciones que son path expressions son validas cuando este valor esta encendido. Las path expression son utilizadas para todas las operaciones de asignación y borrado. La única manera de activar las path expressions es mediante la función builtin `path/1`. Para más información acerca de qué es una path expression, y qué operaciones califican como tal, referirse al manual de jq. Es uno de los conceptos mas complejos del lenguaje.
  - `fr_ctx`: Estructura que define un "contexto de ejecución", es decir, un closure. Cada declaración de funcion o variable tiene asociado un contexto definido cuando se ejecuta su declaración, que es utilizado cada vez que dicha funcion o variable es ejecutada. Una vez que se sale del closure de dicha función o variable, el contexto anterior es restaurado. El contexto es de tipo `FilterRunCtx`, y se compone por:
    - `fr_vars`: Mapa de variables de nombre `Text` a valor `Json`.
    - `fr_funcs`: Mapa de nombre de función y cantidad de parametros `(Text, Int)` a función filtro `FilterFunc`, es decir, una función que recibe los argumentos ordenados de la función `Seq Json` y el input `Json`, y retorna lo mismo que `runFilter`, es decir, `FilterRun (FilterResult (Json, Maybe PathExp))`.
    - `fr_current_func`: Función actual que se esta ejecutando (o `<top_level>/0` si estamos en top level). El nombre (`Text`), cantidad de parámetros (`Int`) y función propiamente dicha (`FilterFunc`). Se utiliza para implementar recursión, es decir, llamar a la misma función que estoy ejecutando.
    - `fr_labels`: Labels declaradas hasta el momento (`HashSet Text`). Utilizado para saber si cierta label existe cuando se ejecuta un `Break`.
    - `fr_file`: Indica si estamos ejecutando el programa propiamente dicho (es decir, top level), o un módulo de funciones. En caso de que sea un módulo, se le indica el nombre.

Ahora estamos en condiciones de entender el tipo de retorno `FilterRun (FilterResult (Json, Maybe PathExp))`. Son todos los resultados retornados por un filtro, cuyo valor exitoso es un `Json` y un `PathExp` sólo en caso de que esté habilitado (`fr_path_exp`) en el estado almacenado por `FilterRun`, la mónada State que envuelve todo.

Puede parecer sorprendente las pocas funcionalidades nativas que se implementan en este módulo. Por ejemplo, brillan por su ausencia las operaciones aritmeticas. Estas y muchas otras funcionalidades y operadores son implementadas a nivel builtins, y el parser únicamente lo que hace es traducir el símbolo a una llamada de función, es decir, mero syntax sugar. Por ejemplo, el símbolo `+`, se traduce a la llamada de función `_plus` durante la generación del AST.

Es por esto último que la funcionalidad de llamada de función (`runFuncDef`, `runFuncCall` y `buildFilterFunc`) es de las más importantes y complejas.

Practicamente ningúna función builtin (en Haskell o jq) debe preocuparse por la generación de path expressions (`PathExp`). Este problema queda practicamente relegado únicamente para las funcionalidades nativas. El resto se apalanca de estas.

#### Data.Filter.Builtins.hs
Este módulo es el encargado de implementar toda la librería estandar de jq, es decir, todas las funciones builtin.

Este módulo exporta una única variable `builtins`, que es un mapa con todas las definiciones de las builtins (`HashMap (Text, Int) FilterFunc`). Estas son luego incorporadas al estado inicial de la ejecución del programa jq, por lo que estan disponibles para el mismo.

Hay 2 maneras distintas de implementar una builtin.
La primera es programarla en Haskell de manera nativa, y declararla a mano. Este es el caso de todas las funciones que se encuentran declaradas en la variable `hsBuiltins`, junto con el nombre y la cantidad de variables que recibe.
La segunda es implementarla directamente en jq, utilizando todas las funcionalidades nativas del lenguaje, todas las builtins implementadas en Haskell y las implementadas previamente en jq. Todas estas funciones estan declaradas en el archivo de recursos `builtins.jq`, que es parseado como un módulo normal para generar la variable `builtins`. El archivo `builtins.jq` es embebido en el ejecutable de `jqhs` en tiempo de compilación como un `ByteString`.

Una función builtin particular es la función `builtins/0`, que retorna un array con todos los nombres de las funciones builtins disponibles. Para implementarla, una vez creada por completo la variable `builtins`, se le agrega esta última función que se encarga de imprimir los nombres de todas las funciones preentes en el mapa.

#### Data.Filter.Internal.Sci.hs
Este pequeño módulo exporta funciones de utilidad a la hora de trabajar con aritmética sobre `Scientific`. El mismo es utilizado tanto por `Data.Filter.Internal.Run.hs` como `Data.Filter.Builtins.hs`.

#### Data.Filter.Internal.Format.hs
Este módulo declara todas las funciones de encoding y decoding de `Json` a otros formatos como csv, tsv, base64, html, etc, principalmente escapando caracteres inválidos.

Estos formatos son los utilizados por la builtin `format/1`, y por ende tambien por los filtros de la forma `@<format>`, que son syntax sugar de llamados a dicha función.

#### Data.Filter.Internal.CMath.hsc
Como es posible que se haya notado, jq reexporta todas las funciones que se pueden encontrar en `math.h`, de la librería estandar de C como builtins. Para poder soportar esto, utilizamos la Foreign Function Interface que ofrece Haskell para importar dichas funciones directamente, y wrappearlas en código haskell mínimo.

En pos de portabilidad, solo importamos las funciones que el estándar de C garantizan que estarán presentes. De otra forma, es posible que el import falle en ciertos sistemas. Las funciones que no son estandar son reimplementadas a partir de otras, o simplemente lanzan un error que indica que no se incluyen pues no son estándar. Por ejemplo, `exp10` o `drem`.

Luego, el módulo de builtins unicamente tiene que agregarlas al mapa de builtins, sin ningún agregado extra.

#### Data.Filter.Internal.CRegex.hsc
Para implementar las builtins de manejo de expresiones regulares, era necesario utilizar una librería que las manejara. Actualmente, Haskell no posee ninguna implementación estandar para ello. Las librerías de uso tradicional son, por lo general, wrapper mediante el FFI de librerías implementadas en C.

Analizando las distintas opciones, se descubrió que no existe actualmente ninguna implemnetación que ofrezca la funcionalidad de named capture groups, sin embargo, las builtins de jq lo requieren.

Fue por estas razones que se consideró viable implementar una wrapper propia mediante de FFI sobre exactamente la misma libraría de expresiones regulares que utiliza JQ, Oniguruma. Se decidió esta ya que es actualmente una de las librerías más utilizadas, extremadamente performante, y que garantiza la mejor compatibilidad con JQ original.

No se entrará demasiado en detalle en la implementación, la cual no es tan sencilla, pero la idea general es la siguiente. El punto de entrada es el método `compile`, que a partir de una regex en formato `ByteString` y flags opcionales de tipo `RegexOpt`, la misma es compilada y encapsulada en el tipo de dato `Regex`, que contiene un puntero abstracto a la estructura que mantiene Oniguruma, y otro puntero a una estructura de datos que se utiliza para generar mensajes de error en caso de fallos. Los mismos son manejados de manera transparente por nuestro lado, el único capaz de acceder a la información dentro de las estructuras son los métodos provistos por Onigurumo.

Luego, a partir de la estructura `Regex`, se ofrece un método `test` que  partir de la regex compilada y un input, retorna un booleano que indica si el input matcheo o no; y un método `match`, que con los mismos parámetros (y un booleano que indica si analizar el primer o todos los matches) retorna una lista de "`Match`", es decir, el offset del match, el largo del match, el string matcheado y una lista con los capture groups (`[MatchCapture]`): en caso de matchear se incluye el offset, largo y string del capture matcheado, y de ser un named capture group, se incluye el nombre.

Toda esta funcionalidad esta luego wrappeada por la builtin "privada" `_match_impl/3`, que parsea los flags provistos, compila la regex, ejecuta la función correspondiente, y luego retorna la información estructurada en objetos `Json`. Todas las demás builtins de expresiones regulares dependen de esta función.

Es muy interesante como a partir de funciones implementadas en C inherentemente impuras, y a partir de manejo de punteros y memoria de stack y heap, se puede construir una abstracción en Haskell totalmente pura y fácil de usar.

## Futuro Trabajo
Por supuesto, este trabajo esta actualmente incompleto. Si bien se considera que las funcionalidades core del lenguaje estan implementadas, distintas features quedan aún por implementar, ya sea por su alta complejidad, por su relativa poca importancia, o simplemente por falta de tiempo.

### Features Faltantes
  - Módulos e Imports: Esta feature fue introducida en la versión 1.6, la cual permite buscar módulos externos con declaraciones de funciones y luego cargarlas según se necesiten mediante nuevas keywords del lenguaje de importación. Esta funcionalidad es, por supuesto, bastante compleja. Las builtins `modulemeta/0` y `get_search_list/0` tampoco fueron implementadas, pues sólo tienen sentido en un contexto donde esta feature este implementada.
  - Destructuring: Para mantener el parseo lo más simple posible, y ya que esta funcionalidad únicamente brinda syntax sugar, la implementación de esta feature fue empujada hacia el futuro. Esto también incluye el operador `?//`, el cual solo tiene sentido en un contexto donde esta feature este implementada.
  - Chequeos en tiempo de compilacion de la existencia de identificadores (variables, funciones y labels): jq falla en tiempo de compilación en caso de que un identificador previamente no declarado es utilizado. Actualmente, jqhs no provee esta feature tan útil. Únicamente falla en tiempo de ejecución si el identificador no declarado en ejecutado.
  - Buenos mensajes de errores de compilación y ejecución: jq hace un esfuerzo por proveer mensajes de error que ayuden al usuario a trackear el error. Esta feature esta parcialmente faltante en errores de tiempo de ejecución, y totalmente ausente en errores de compilación. Actualmente, el desarrolo no está centrado en esta área.

### Builtins Faltantes
Actualmente `197/216` builtins están implementadas. Las funciones faltantes son:
  - Funciones de Fechas. Estas funciones son un fino wrapper de las funciones de tiempo POSIX, por lo que acceso directo a las funciones C mediante la FFI parece un método sencillo para implementarlas, sin embargo esto requeriría lidiar con los distintos comportamientos dependientes del sistema asociados a estas funciones. Reimplementarlas en Haskell es problablemente el camino mas sensato, sin embargo la falta de un librería de tiempo completa y estándar en Haskell lo hace más complejo de lo que parecería en un principio. Las funciones son:
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

  - Funciones de IO. Actualmente hay 2 tipos de funciones de IO faltantes. El primer tipo son las que consumen los input disponibles desde dentro del programa jq. Su implementación estaba en desarrollo, pero se retrasó debido a conflictos con cómo está implementado la recursion de funciones. La idea sería almacenar el inputs en el estado de ejecucíon de los filtros, e ir consumiendolos de ahí. El segundo tipo son las que tienen acceso a la información del archivo fuente (numero de línea, nombre de archivo, etc) del input que se está procesando. Ya que, actualmente, jqhs trata a los inputs como `Json`s, sin guardar metadata relacionada a su origen, no es posible implementarlas actualmente. Mas aún, no está claro que la inclusion de dichas funciones esotéricas justifique el cambio arquitectural que representaría, con todas sus contras. Las funciones son:
    - `input/0`
    - `inputs/0`
    - `input_filename/0`
    - `input_line_number/0`

  - Funciones de Environment. Estas funciones obtienen información sobre el contexto de ejecución de la aplicación. Estas funciones estan pobremente documentadas e inestables. No se implementan por decisión. Las funciones son:
    - `get_prog_origin/0`
    - `get_jq_origin/0`

  - Módulos e Imports. Ya que la feature de módulos no está implementada, las builtins asociadas a esta feature tampoco se incluyen. Estas funciones son:
    - `modulemeta/0`
    - `get_search_list/0`

### Opciones del Ejecutable Faltantes
No todas las opciones que jq ofrece mediante la línea de comandos son ofrecidaspor jqhs. Las opciones faltantes son:
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
