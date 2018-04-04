{-# LANGUAGE MultiParamTypeClasses #-}

module Documentation.Haddock.Parser.Markdown where

import Commonmark
import Commonmark.Extensions.Math
import Commonmark.Extensions.PipeTable

import           Documentation.Haddock.Doc
import           Documentation.Haddock.Types
import qualified Data.Text as T

instance (Show mod, Show id) => IsInline (DocH mod id) where
  softBreak = DocString "\n"
  str t = DocString (T.unpack t)
  escapedChar c = DocString [c]
  emph = DocEmphasis
  strong = DocBold
  link dest _ (DocString s) = DocHyperlink (Hyperlink (T.unpack dest) (Just s))
  code txt = DocMonospaced (DocString (T.unpack txt))
  image dest title _ = DocPic (Picture (T.unpack dest) (Just (T.unpack title)))
  
  entity = undefined -- :: Text -> DocH mod id
  lineBreak = undefined -- :: DocH mod id
  rawInline = undefined -- :: Format -> Text -> DocH mod id

instance Semigroup (DocH mod id) where
  (<>) = DocAppend

instance Monoid (DocH mod id) where
  mappend = DocAppend
  mempty = DocEmpty

instance Rangeable (DocH mod id) where
  ranged _ x = x 

instance (Show mod, Show id) => IsBlock (DocH mod id) (DocH mod id) where
  paragraph = DocParagraph
  plain = id
  codeBlock _ cb = DocCodeBlock (DocString (T.unpack cb))
  header lvl x = DocHeader (Header lvl x)
  list t _ = case t of { BulletList{} -> DocUnorderedList; OrderedList{} -> DocOrderedList }
            
  thematicBreak = undefined -- :: DocH mod id
  blockQuote = undefined -- :: DocH mod id -> DocH mod id
  rawBlock = undefined -- :: Format -> Text -> DocH mod id
  referenceLinkDefinition = undefined -- :: Text {-^ Label } -> (Text, Text) {-^ Destination, title -} -> DocH mod id

instance HasMath (DocH mod id) where
  inlineMath = DocMathInline . T.unpack
  displayMath = DocMathDisplay . T.unpack

{-
instance HasPipeTable (DocH mod id) (DocH mod id) where
  pipeTable _ hds rst = Table { tableHeaderRows = [ TableRow (map (TableCell 1 1) hds) ]
                              , tableBodyRows   = [ TableRow (map (TableCell 1 1) bdr) | bdr <- rst ]
                              }
-}


{-
 data DocH mod id
  = DocEmpty
  | DocAppend (DocH mod id) (DocH mod id)
  | DocString String
 
    | DocParagraph (DocH mod id)
  | DocIdentifier id
  | DocIdentifierUnchecked mod
  | DocModule String
  | DocWarning (DocH mod id)
    | DocEmphasis (DocH mod id)
    | DocMonospaced (DocH mod id)
    | DocBold (DocH mod id)
    | DocUnorderedList [DocH mod id]
    | DocOrderedList [DocH mod id]
  | DocDefList [(DocH mod id, DocH mod id)]
    | DocCodeBlock (DocH mod id)
    | DocHyperlink Hyperlink
    | DocPic Picture
    | DocMathInline String
    | DocMathDisplay String
  | DocAName String
  | DocProperty String
  | DocExamples [Example]
    | DocHeader (Header (DocH mod id))
    | DocTable (Table (DocH mod id))
  deriving (Eq, Show, Functor, Foldable, Traversable)

 -}
