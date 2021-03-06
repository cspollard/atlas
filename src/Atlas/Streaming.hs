{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Atlas.Streaming
  ( decompress, compress, serializeP, deserializeP
  , encodeFile, decodeFile, addFiles, decodeFiles, decodeFiles'
  , filterFolder
  ) where

import           Atlas.ProcessInfo
import           Atlas.Variation
import           Control.Applicative        (liftA2)
import           Control.Exception          (throwIO)
import           Control.Lens
import           Control.Monad              (forever, unless)
import           Control.Monad.State.Strict hiding (get, put)
import qualified Data.ByteString            as BS
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
import qualified Pipes.Prelude              as P
import           System.IO                  (IOMode (..), hFlush, stdout,
                                             withFile)
import           Text.Regex.Base.RegexLike
import           Text.Regex.Posix.String


addFiles
  :: (ProcessInfo, Sum Double, Folder (Annotated (Vars Obj)))
  -> (ProcessInfo, Sum Double, Folder (Annotated (Vars Obj)))
  -> Either String (ProcessInfo, Sum Double, Folder (Annotated (Vars Obj)))
addFiles (i, w, f) (i', w', f')
  | i /= i' = Left "attempting to add two different dsids!"
  | otherwise =
    let w'' = w <> w'
        f'' = f <> f'
    in seq w'' . seq f'' $ Right (i, w'', f'')


encodeFile :: String -> (ProcessInfo, Sum Double, Folder (Annotated (Vars Obj))) -> IO ()
encodeFile fname (i, w, f) = do
  putStrLn ("encoding file " ++ fname) >> hFlush stdout
  withFile fname WriteMode $ \h ->
    runEffect
    $ PBS.toHandle h
      <-<
        compress
        ( do
          yield (encode i)
          yield (encode w)
          serializeP <-< P.each (M.toList $ _toMap f)
        )


decodeFile
  :: [String]
  -> [String]
  -> String
  -> IO (Either String (ProcessInfo, Sum Double, Folder (Annotated (Vars Obj))))
decodeFile rxps nxps fname = do
  putStrLn ("decoding file " ++ fname)
  hFlush stdout
  withFile fname ReadMode $ \h ->
    runEffect $ do
      mx <- deser . decompress $ PBS.fromHandle h
      case mx of
        Just (i, w, p) -> do
          fol <-
            P.fold (flip $ uncurry M.insert) M.empty Folder
            $ P.filter filt <-< void p
          i `seq` w `seq` fol `seq` (return $ Right (i, w, fol))
        Nothing -> return . Left $ "failed to parse file " ++ fname

  where
    filtrx s = all (\rx -> matchRegex rx s) rxps
    filtnx s = not $ any (\rx -> matchRegex rx s) nxps
    filt x =
      let s = T.unpack $ fst x
      in filtrx s && filtnx s


    deser p = do
      (mi, p') <- runStateT deserializeP p
      (mw, p'') <- runStateT deserializeP p'
      return $ (,,) <$> mi <*> mw <*> Just (P.parsed_ deserializeP p'')


decodeFiles
  :: Foldable f
  => [String]
  -> [String]
  -> f String
  -> IO (Either String (StrictMap ProcessInfo (Sum Double, Folder (Annotated (Vars Obj)))))
decodeFiles rxp nxp infs =
  P.fold (liftA2 add) (Right mempty) id
  $ P.mapM (decodeFile rxp nxp) <-< P.each infs

  where
    add
      :: StrictMap ProcessInfo (Sum Double, Folder (Annotated (Vars Obj)))
      -> (ProcessInfo, Sum Double, Folder (Annotated (Vars Obj)))
      -> StrictMap ProcessInfo (Sum Double, Folder (Annotated (Vars Obj)))
    add sm (proci, w, fol) = sm & at proci %~ f (w, fol)
    f y Nothing  = Just y
    f y (Just x) = Just $ x `mappend` y


-- decode files with the understanding that they should
-- all come from the same sample
decodeFiles'
  :: Foldable f
  => [String]
  -> [String]
  -> f String
  -> IO (Either String (ProcessInfo, Sum Double, Folder (Annotated (Vars Obj))))
decodeFiles' rxp nxp infs =
  P.fold add Nothing (fromMaybe $ Left "no files decoded!")
  $ P.mapM (decodeFile rxp nxp) <-< P.each infs

  where
    add Nothing y                  = Just $! y
    add (Just (Right x)) (Right y) = Just $! addFiles x y
    add x@(Just (Left _)) _        = x
    add (Just _) x@(Left _)        = Just $! x


-- a lot of code taken from
-- https://hackage.haskell.org/package/pipes-zlib-0.4.4.1/docs/src/Pipes-Zlib.html
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
  => Proxy x' x () BS.ByteString m r
  -> Proxy x' x () BS.ByteString m r
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
  => Proxy x' x () BS.ByteString m r
  -> Proxy x' x () BS.ByteString m r
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
deserializeP = go Nothing
  where
    go mk = do
      bin <- fromMaybe BS.empty <$> P.draw
      case fromMaybe (runGetPartial get) mk bin of
        Fail _reason _leftover -> return Nothing
        Partial k -> go (Just k)
        Done c bin' -> do
          when (bin' /= BS.empty) $ P.unDraw bin'
          return $ Just c


-- | Serialize data into strict ByteStrings.
serializeP :: (Serialize a, Monad m) => Pipe a BS.ByteString m ()
serializeP = forever $ do
    x <- await
    yield (encode x)


matchRegex :: String -> String -> Bool
matchRegex rxp = matchTest (makeRegex rxp :: Regex)

filterFolder :: Maybe String -> Folder a -> Folder a
filterFolder s f = maybe f (`g` f) s
  where
    g s' =
      let rxp = makeRegex s' :: Regex
          h k _ = matchTest rxp $ T.unpack k
      in inF (M.filterWithKey h)
