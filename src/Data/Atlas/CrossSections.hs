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

crossSectionInfo :: Parser CrossSectionInfo
crossSectionInfo = fromList . rights <$>
                    (skipSpace *> many (eitherP comment xsecline) <* endOfInput)
    where
        comment = char '#' *> takeTill isEndOfLine *> skipSpace
        xsecline = (,) <$> (decimal <* skipSpace)
                       <*> ((*) <$> (double <* skipSpace)
                                <*> option 1.0 (double <* skipSpace))


readXSecFile :: FilePath -> IO (Maybe CrossSectionInfo)
readXSecFile f = do
    bs <- BS.readFile f
    g (parse crossSectionInfo bs)

    where
        g x = case x of
                Done _ r -> return (Just r)
                Partial h -> g $ h ""
                Fail i contexts err -> do
                    putStrLn "remaining input:"
                    print i
                    putStrLn "contexts:"
                    print contexts
                    putStrLn "error: "
                    putStrLn err
                    return Nothing
