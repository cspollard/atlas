{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Atlas.CrossSections where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 hiding (isEndOfLine)
import qualified Data.ByteString                  as BS
import           Data.Either                      (rights)
import           Data.IntMap

isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\n' || c == '\r'

type CrossSectionInfo = IntMap (Double, String)

crossSectionInfo :: Parser CrossSectionInfo
crossSectionInfo = fromList . rights <$>
                    (skipSpace *> many (eitherP comment xsecline) <* endOfInput)
    where
        comment = char '#' *> takeTill isEndOfLine *> skipSpace
        xsecline =
            (,) <$> dsid <*> ( (,) <$> xsec <*> shower )


        dsid = decimal <* skipSpace
        xsec = (*) <$> (double <* skipSpace) <*> (double <* skipSpace)
        shower = many1 (letter_ascii <|> digit) <* skipSpace


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
