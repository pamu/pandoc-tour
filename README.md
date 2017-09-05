# Pandoc tour
---------------

### ___Pandoc tour goals___ 
- Exploring Pandoc
- Using pandoc as lib instead of commandline executable
- Retrieving information from pandoc markdown parse tree

### Exploring Pandoc
--------------------

- ___Text.Pandoc___ module contains readers and writers for different kind of formats. Here are the exposed functions and data definitions in Text.Pandoc package

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

<h3 id="hello-world">Hello world</h3>%  

```

Note that `###Helloworld` markdown string is converted to html string 

### Retrieving Meta information from pandoc markdown parse tree

Meta information about the markdown document is written in the form of yaml format.

Pandoc markdown parser parses the yaml meta information and keeps it in the meta section of the ```Pandoc``` record

```haskell
    data Pandoc = Pandoc Meta [Block]
```

Meta contains meta information about the pandoc document written in the yaml format.

```haskell
-- | Metadata for the document:  title, authors, date.
newtype Meta = Meta { unMeta :: M.Map String MetaValue }
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

data MetaValue = MetaMap (M.Map String MetaValue)
               | MetaList [MetaValue]
               | MetaBool Bool
               | MetaString String
               | MetaInlines [Inline]
               | MetaBlocks [Block]
               deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)
               
-- | Inline elements.
data Inline
    = Str String            -- ^ Text (string)
    | Emph [Inline]         -- ^ Emphasized text (list of inlines)
    | Strong [Inline]       -- ^ Strongly emphasized text (list of inlines)
    | Strikeout [Inline]    -- ^ Strikeout text (list of inlines)
    | Superscript [Inline]  -- ^ Superscripted text (list of inlines)
    | Subscript [Inline]    -- ^ Subscripted text (list of inlines)
    | SmallCaps [Inline]    -- ^ Small caps text (list of inlines)
    | Quoted QuoteType [Inline] -- ^ Quoted text (list of inlines)
    | Cite [Citation]  [Inline] -- ^ Citation (list of inlines)
    | Code Attr String      -- ^ Inline code (literal)
    | Space                 -- ^ Inter-word space
    | SoftBreak             -- ^ Soft line break
    | LineBreak             -- ^ Hard line break
    | Math MathType String  -- ^ TeX math (literal)
    | RawInline Format String -- ^ Raw inline
    | Link Attr [Inline] Target  -- ^ Hyperlink: alt text (list of inlines), target
    | Image Attr [Inline] Target -- ^ Image:  alt text (list of inlines), target
    | Note [Block]          -- ^ Footnote or endnote
    | Span Attr [Inline]    -- ^ Generic inline container with attributes
    deriving (Show, Eq, Ord, Read, Typeable, Data, Generic)
    
ata Block
    = Plain [Inline]        -- ^ Plain text, not a paragraph
    | Para [Inline]         -- ^ Paragraph
    | LineBlock [[Inline]]  -- ^ Multiple non-breaking lines
    | CodeBlock Attr String -- ^ Code block (literal) with attributes
    | RawBlock Format String -- ^ Raw block
    | BlockQuote [Block]    -- ^ Block quote (list of blocks)
    | OrderedList ListAttributes [[Block]] -- ^ Ordered list (attributes
                            -- and a list of items, each a list of blocks)
    | BulletList [[Block]]  -- ^ Bullet list (list of items, each
                            -- a list of blocks)
    | DefinitionList [([Inline],[[Block]])]  -- ^ Definition list
                            -- Each list item is a pair consisting of a
                            -- term (a list of inlines) and one or more
                            -- definitions (each a list of blocks)
    | Header Int Attr [Inline] -- ^ Header - level (integer) and text (inlines)
    | HorizontalRule        -- ^ Horizontal rule
    | Table [Inline] [Alignment] [Double] [TableCell] [[TableCell]]  -- ^ Table,
                            -- with caption, column alignments (required),
                            -- relative column widths (0 = default),
                            -- column headers (each a list of blocks), and
                            -- rows (each a list of lists of blocks)
    | Div Attr [Block]      -- ^ Generic block container with attributes
    | Null                  -- ^ Nothing
    deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)
               
```

Above definitions are hidden in the ```pandoc``` lib. In order to access above definitions ```pandoc-types``` lib must be added to the dependencies.

Using above definitions these functions are written to access the title, summary, date and tags of the markdown document.

```markdown

---
  title : hello world
  tags: [java, scala, haskell]
  date: 02-09-2017
  summary:
        Scala is a eagerly evaluated, typesafe functional programming
        language widely used by many companies all
        over the world. Haskell is non-strict, typesafe functional programming language with great type-inferring compiler. 
        
---


### Hello world
Hello world is the widely used string in the programming community.

```

Meta functions can be used to retrieve this meta information of the above markdown document.

![Meta info retrieval functions are here in Meta module](https://github.com/pamu/pandoc-tour/blob/master/src/Meta.hs)
