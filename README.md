# Pandoc tour
---------------

### ___Pandoc tour goals___ 
- Exploring Pandoc
- Using pandoc as lib instead of commandline executable
- Retrieving information from pandoc markdown parse tree
- Converting markdown to html

### Exploring Pandoc
--------------------

___Text.Pandoc___ module contains readers and writers for different kind of formats. Here are the exposed functions and definitions in Text.Pandoc package

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

![Look at this code snippet to know](/src/Explore.hs)

So, Pandoc does three things
- Parse different formats (using readers provided for different formats)
- Successful parsing with result in AST (abstract syntax tree)
- Convert the AST to another format (output format)
- Write the resultant bytes into the output file with appropriate file extension



