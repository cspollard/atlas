-- {-# LANGUAGE FlexibleContexts    #-}
-- {-# LANGUAGE OverloadedStrings   #-}
-- {-# LANGUAGE RankNTypes          #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TupleSections       #-}

module Atlas.ToYoda where
--   ( scaleH, variationFromMap, decodeFile, decodeFiles
--   , ProcMap, mainWith
--   ) where
--
-- import           Atlas
-- import           Atlas.CrossSections
-- import           Atlas.ProcessInfo
-- import           Control.Lens
-- import qualified Data.IntMap.Strict  as IM
-- import qualified Data.Map.Strict     as M
-- import           Data.Maybe          (fromMaybe)
-- import           Data.Monoid         hiding ((<>))
-- import           Data.Semigroup      ((<>))
-- import           Options.Applicative
--
-- type ProcMap = IM.IntMap
--
-- scaleH :: Double -> Obj -> Obj
-- scaleH x (H1DD h) = H1DD $ scaling x h
-- scaleH x (P1DD h) = P1DD $ scaling x h
-- scaleH x (H2DD h) = H2DD $ scaling x h
--
-- data InArgs =
--   InArgs
--     { outfolder :: String
--     , xsecfile  :: String
--     , lumi      :: Double
--     , regex     :: Maybe String
--     , infiles   :: [String]
--     }
--
-- inArgs :: Parser InArgs
-- inArgs = InArgs
--   <$> strOption
--     ( long "outfolder"
--     <> short 'o'
--     <> metavar "OUTFOLDER" )
--   <*> strOption
--     ( long "xsecfile"
--     <> metavar "XSECFILE" )
--   <*> option auto
--     ( long "lumi"
--     <> metavar "LUMI" )
--   <*> optional
--     ( strOption (long "regex" <> metavar "REGEX=\".*\"") )
--   <*> some (strArgument (metavar "INFILES"))
--
-- opts :: ParserInfo InArgs
-- opts = info (helper <*> inArgs) fullDesc
--
-- -- TODO
-- -- partial!
-- variationFromMap :: Ord k => k -> M.Map k a -> Variations (M.Map k) a
-- variationFromMap k m =
--   let n = m M.! k
--   in Variations n $ sans k m
--
-- mainWith
--   :: (Double -> String -> ProcMap (VarsT m (Folder YodaObj)) -> IO ()) -> IO ()
-- mainWith writeFiles = do
--   args <- execParser opts
--
--   xsecs <-
--     fromMaybe (error "failed to parse xsec file.")
--       <$> (fmap.fmap.fmap) fst (readXSecFile (xsecfile args))
--
--   epm <- decodeFiles (regex args) (infiles args)
--   case epm of
--     Left err -> error err
--     Right procmap -> do
--       -- TODO
--       -- so much traverse.....
--       let procmap' =
--             flip IM.mapWithKey procmap
--             $ \ds (Sum w, hs) ->
--               if ds == 0
--                 then
--                   hs
--                     & over (traverse.traverse.annots)
--                       ( (at "LineStyle" ?~ "solid")
--                       . (at "LineColor" ?~ "Black")
--                       . (at "DotSize" ?~ "0.15")
--                       . (at "ErrorBars" ?~ "1")
--                       . (at "PolyMarker" ?~ "*")
--                       . (at "Title" ?~ "\"data\"")
--                       )
--                 else
--                   let t = ("\"" <> processTitle ds <> "\"")
--                   in hs
--                       & over
--                         (traverse.traverse)
--                         ( over noted (scaleH (xsecs IM.! ds / w))
--                           . set (annots.at "Title") (Just t)
--                         )
--
--       writeFiles (lumi args) (outfolder args) procmap'
