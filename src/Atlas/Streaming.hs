{-# LANGUAGE RankNTypes #-}

module Atlas.Streaming
  ( decompress, compress, serializeP, deserializeP
  , encodeFile, decodeFile, addFiles, decodeFiles, decodeFiles'
  ) where

import           Atlas.Histogramming
import           Atlas.Variation
import           Control.Applicative        (liftA2)
import           Control.Exception          (throwIO)
import           Control.Monad              (forever, unless)
import           Control.Monad.State.Strict hiding (get)
import qualified Data.ByteString            as BS
import qualified Data.IntMap.Strict         as IM
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (Sum (..), (<>))
import           Data.Serialize             hiding (flush)
import           Data.Streaming.Zlib        as Z
import qualified Data.Text                  as T
import           Data.YODA.Obj
import           Pipes                      as P
import qualified Pipes.ByteString           as PBS
import qualified Pipes.Parse                as P
import           Pipes.Prelude              as P
import           System.IO                  (IOMode (..), hFlush, stdout,
                                             withFile)

addFiles
  :: (Int, Sum Double, Folder (Vars YodaObj))
  -> (Int, Sum Double, Folder (Vars YodaObj))
  -> Either String (Int, Sum Double, Folder (Vars YodaObj))
addFiles (i, w, f) (i', w', f')
  | i /= i' = Left "attempting to add two different dsids!"
  | otherwise = Right (i, w <> w', f <> f')

encodeFile :: String -> (Int, Sum Double, Folder (Vars YodaObj)) -> IO ()
encodeFile fname (i, w, f) = do
  putStrLn ("encoding file " ++ fname) >> hFlush stdout
  withFile fname WriteMode $ \h ->
    runEffect
    $ PBS.toHandle h
      <-< compress serializeP
      <-<
        ( do
          yield (encode i)
          yield (encode w)
          yield (encode . M.toList $ folderToMap f)
        )

decodeFile
  :: Maybe String
  -> String
  -> IO (Either String (Int, Sum Double, Folder (Vars YodaObj)))
decodeFile rxp fname = do
  putStrLn ("decoding file " ++ fname) >> hFlush stdout
  withFile fname ReadMode $ \h ->
    runEffect $ do
      mx <- deser . decompress $ PBS.fromHandle h
      case mx of
        Just (i, w, p) -> do
          fol <-
            P.fold (flip $ uncurry M.insert) M.empty Folder
            $ P.filter filt <-< void p
          return . Right $ (i, w, fol)
        Nothing -> return . Left $ "failed to parse file" ++ fname

  where
    filt = fromMaybe (const True) (matchRegex <$> rxp) . T.unpack . fst
    deser p = do
      (mi, p') <- runStateT deserializeP p
      (mw, p'') <- runStateT deserializeP p'
      return $ (,,) <$> mi <*> mw <*> pure (P.parsed_ deserializeP p'')


decodeFiles
  :: Foldable f
  => Maybe String
  -> f String
  -> IO (Either String (IM.IntMap (Sum Double, Folder (Vars YodaObj))))
decodeFiles rxp infs =
  P.fold (liftA2 add) (Right IM.empty) id
  $ P.mapM (decodeFile rxp) <-< P.each infs

  where
    add im (i, w, fol) = IM.insertWith mappend i (w, fol) im


-- decode files with the understanding that they should all have *the same dsid*
decodeFiles'
  :: Foldable f
  => Maybe String
  -> f String
  -> IO (Either String (Int, Sum Double, Folder (Vars YodaObj)))
decodeFiles' rxp infs =
  P.fold add Nothing (fromMaybe $ Left "no files decoded!")
  $ P.mapM (decodeFile rxp) <-< P.each infs

  where
    add Nothing y                  = Just y
    add (Just (Right x)) (Right y) = Just $ addFiles x y
    add x@(Just (Left _)) _        = x
    add (Just _) x@(Left _)        = Just x

-- a lot of code taken from https://hackage.haskell.org/package/pipes-zlib-0.4.4.1/docs/src/Pipes-Zlib.html
gzWindowBits :: WindowBits
gzWindowBits = Z.WindowBits 31

fromPopper :: MonadIO m
           => Z.Popper
           -> Producer' BS.ByteString m ()
fromPopper pop = go
  where
    go = do
      mbs <- liftIO pop
      case mbs of
          PRDone    -> return ()
          PRError e -> liftIO $ throwIO e
          PRNext bs -> yield bs >> go

decompress
  :: MonadIO m
  => Proxy x' x () BS.ByteString m r -- ^ Compressed stream
  -> Proxy x' x () BS.ByteString m r -- ^ Decompressed stream
decompress p0 = do
    inf <- liftIO $ Z.initInflate gzWindowBits
    r <- for p0 $ \bs -> do
       popper <- liftIO (Z.feedInflate inf bs)
       fromPopper popper
    bs <- liftIO $ Z.finishInflate inf
    unless (BS.null bs) (yield bs)
    return r

compress
  :: MonadIO m
  => Proxy x' x () BS.ByteString m r -- ^ Decompressed stream
  -> Proxy x' x () BS.ByteString m r -- ^ Compressed stream
compress p0 = do
    def <- liftIO $ Z.initDeflate (-1) gzWindowBits
    r <- for p0 $ \bs -> do
       popper <- liftIO (Z.feedDeflate def bs)
       fromPopper popper
    fromPopper $ Z.finishDeflate def
    return r

deserializeP
  :: (Serialize a, Monad m)
  => P.Parser BS.ByteString m (Maybe a)
deserializeP = go Nothing Nothing
  where
    go mk mbin = do
      bin <- maybe (fromMaybe BS.empty <$> P.draw) return mbin
      case fromMaybe (runGetPartial get) mk bin of
        Fail _reason _leftover ->
          return Nothing
        Partial k ->
          go (Just k) Nothing
        Done c bin' -> do
          P.unDraw bin'
          return $ Just c


-- | Serialize data into strict ByteStrings.
serializeP :: (Serialize a, Monad m) => Pipe a PBS.ByteString m ()
serializeP = forever $ do
    x <- await
    yield (encode x)
