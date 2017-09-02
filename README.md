# Pandoc tour
---------------

### ___Pandoc tour goals___ 
- Exploring Pandoc
- Using pandoc as lib instead of commandline executable
- Retrieving information from pandoc markdown parse tree

### Exploring Pandoc
--------------------

___Text.Pandoc___ module contains readers and writers for different kind of formats. Here are the exposed functions and data definitions in Text.Pandoc package

```haskell
                Reader (..)
               , mkStringReader
               , readDocx
               , readOdt
               , readMarkdown
               , readCommonMark
               , readMediaWiki
               , readRST
               , readOrg
               , readLaTeX
               , readHtml
               , readTextile
               , readDocBook
               , readOPML
               , readHaddock
               , readNative
               , readJSON
               , readTWiki
               , readTxt2Tags
               , readTxt2TagsNoMacros
               , readEPUB
               -- * Writers: converting /from/ Pandoc format
              , Writer (..)
               , writeNative
               , writeJSON
               , writeMarkdown
               , writePlain
               , writeRST
               , writeLaTeX
               , writeConTeXt
               , writeTexinfo
               , writeHtml
               , writeHtmlString
               , writeICML
               , writeDocbook
               , writeOPML
               , writeOpenDocument
               , writeMan
               , writeMediaWiki
               , writeDokuWiki
               , writeZimWiki
               , writeTextile
               , writeRTF
               , writeODT
               , writeDocx
               , writeEPUB
               , writeFB2
               , writeOrg
               , writeAsciiDoc
               , writeHaddock
               , writeCommonMark
               , writeCustom
               , writeTEI 
```

- ``readMarkdown`` is used to read and parse markdown to convert into the pandoc AST.
- Now any writer can be used to convert the produced AST into another format.

![Look at this code snippet to know](https://github.com/pamu/pandoc-tour/blob/master/src/Explore.hs)

So, Pandoc does three things
- Parse different formats (using readers provided for different formats)
- Successful parsing with result in AST (abstract syntax tree)
- Convert the AST to another format (output format)
- Write the resultant bytes into the output file with appropriate file extension

### Using pandoc as lib
-----------------------

Pandoc is available as both haskell library and also as commandline tool

```cabal

To use pandoc as library add pandoc to cabal dependencies 

executable pandoc-tour-exe
  hs-source-dirs:      app
  main-is:             Main.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
  build-depends:       base
                     , pandoc-tour
                     , pandoc
                     , text
  default-language:    Haskell2010

```

Using pandoc as library helps in building some other useful tools around pandoc readers and writers like blogs which can written using markdown instead of not so desirable html.

![Look at this code snippet to know](https://github.com/pamu/pandoc-tour/blob/master/app/Main.hs)

Interact can be helpful in building custom commandline tools which does conversion

interact function takes a converter as argument and returns a program which reads input from input stream and outputs the output on output stream 

```haskell
interact :: (String -> String) -> IO ()
```

Note that ![Pandoc tour](https://github.com/pamu/pandoc-tour) is a stack project. So, Pandoc tour can be built using `stack`

```bash
stack build

stack install

echo "###Hello world" | pandoc-tour-exe                                    

<h3 id="hello-world">Hello world</h3>%  # output
``` 

### Retrieving information from pandoc markdown parse tree

