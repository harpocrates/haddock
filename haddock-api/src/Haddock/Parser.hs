{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving
             , FlexibleInstances, UndecidableInstances
             , IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module      :  Haddock.Parser
-- Copyright   :  (c) Mateusz Kowalczyk 2013,
--                    Simon Hengel      2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable

module Haddock.Parser ( parseParas
                      , parseString
                      , parseIdent
                      ) where

import qualified Documentation.Haddock.Parser as P
import DynFlags (DynFlags)
import FastString (mkFastString)
import Documentation.Haddock.Types
import Haddock.Types (NsRdrName(..))
import Lexer (mkPState, unP, ParseResult(POk))
import Parser (parseIdentifier)
import SrcLoc (mkRealSrcLoc, GenLocated(L), Located, SrcSpan)
import StringBuffer (stringToStringBuffer)

parseParas :: DynFlags -> Maybe Package -> Located String -> MetaDoc mod NsRdrName
parseParas d p (L sp s) = overDoc (P.overIdentifier (parseIdent d sp)) . P.parseParas p $ s

parseString :: DynFlags -> Located String -> DocH mod NsRdrName
parseString d (L sp s) = P.overIdentifier (parseIdent d sp) . P.parseString $ s

parseIdent :: DynFlags
           -> SrcSpan       -- ^ span of the initial docstring
           -> Namespace     -- ^ namespace of the identifier (value, type, none)
           -> String        -- ^ identifier content
           -> Maybe NsRdrName
parseIdent dflags sp ns str0 =
  let buffer = stringToStringBuffer str0
      realSrcLc = mkRealSrcLoc (mkFastString "<unknown file>") 0 0
      pstate = mkPState dflags buffer realSrcLc
  in case unP parseIdentifier pstate of
    POk _ (L _ name) -> Just (NsRdrName ns name sp)
    _ -> Nothing
