{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.ProcessInfo where

import Data.Text (Text)

import Data.Monoid ((<>))

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
                | btwn 361300 361323 ds = "Wenu" <> hf ds
                | btwn 361324 361347 ds = "Wmunu" <> hf ds
                | btwn 361348 361371 ds = "Wtaunu" <> hf ds
                | btwn 361372 361395 ds = "Zee" <> hf ds
                | btwn 361396 361419 ds = "Zmumu" <> hf ds
                | btwn 361420 361443 ds = "Ztautau" <> hf ds
                | btwn 361444 361462 ds = "Znunu" <> hf ds
                | btwn 410064 410065 ds = "Wt"
                | otherwise             = "other"
