{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.CrossSections where

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8 hiding (isEndOfLine)
import Data.IntMap
import Data.Either (rights)

isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'

type CrossSectionInfo = IntMap Double

-- the first bit guarantees that data always has a cross section of 1.
crossSectionInfo :: Parser CrossSectionInfo
crossSectionInfo = fromList . ((0, 1.0) :) . rights <$>
                    (skipSpace *> many (eitherP comment xsecline) <* endOfInput)
    where
        comment = char '#' *> takeTill isEndOfLine *> skipSpace
        xsecline = (,) <$> (decimal <* skipSpace)
                       <*> ((*) <$> (double <* skipSpace)
                                <*> option 1.0 (double <* skipSpace))


readXSecFile :: FilePath -> IO (Maybe CrossSectionInfo)
readXSecFile f = do
    bs <- BS.readFile f
    return . maybeResult $ parse crossSectionInfo bs
