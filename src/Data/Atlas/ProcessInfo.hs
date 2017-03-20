{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.ProcessInfo where
import           Data.Monoid ((<>))
import           Data.Text   (Text)

btwn :: Int -> Int -> Int -> Bool
btwn mn mx x = mn <= x && x <= mx

hf :: Int -> Text
hf x = case (x `mod` 3) `compare` 1 of
            LT -> "B"
            EQ -> "L"
            GT -> "C"

processTitle :: Int -> Text
processTitle ds | ds == 0               = "data"
                | ds == 410000          = "Pow+Py (nominal)"
                | ds == 410001          = "Pow+Py (radHi)"
                | ds == 410002          = "Pow+Py (radLo)"
                | ds == 410003          = "aMC+H++"
                | ds == 410004          = "Pow+H++"
                | btwn 363388 363411 ds = "Zee" <> hf ds
                | btwn 363364 363387 ds = "Zmumu" <> hf ds
                | btwn 363102 363122 ds = "Ztautau" <> hf ds
                | btwn 363361 363363 ds = "Ztautau" <> hf ds
                | btwn 410147 410148 ds = "Wt"
                | otherwise             = "other"
