{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Glug.SrtExtract (
  parseSrtFromZip
)
where

import qualified Codec.Archive.Zip as Z
import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import qualified Data.Text.Lazy.Encoding as Enc
import qualified Data.Text.Encoding.Error as Enc
import qualified Data.Time.Clock as C
import qualified Text.Subtitles.SRT as SRT

import Data.List (find)
import Data.Attoparsec.Text (parseOnly)
import Control.Applicative ((<|>))
import Control.Exception (try, evaluate)
import Control.Monad.Except (MonadError, catchError, throwError)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Glug.Types (Subtitle (..))
import Glug.Monad (hoistEither, hoistMaybe)


parseSrtFromZip :: MonadError String m => B.ByteString -> m [Subtitle]
parseSrtFromZip zipbs = do
      srtbs <- getSrtBS zipbs
      cleanupSubs . map toSubtitle <$> bsToSubs srtbs
    where toSubtitle :: SRT.Line -> Subtitle
          toSubtitle (SRT.Line _ (SRT.Range f _) _ d) = Subtitle
                                  { dialogue = d
                                  , timestamp = toDiffTime f }


bsToSubs :: MonadError String m => B.ByteString -> m SRT.Subtitles
bsToSubs bs = do
    t <- tag "decoding" . hoistEither . eitherShow . decode' $ bs
    tag "using srt parser" . hoistEither . parseOnly SRT.parseSRT . T.toStrict $ t
  where bom = B.unpack $ B.take 3 bs
        decode | take 2 bom == [0xFE, 0xFF] = Enc.decodeUtf16BE . B.drop 2
               | take 2 bom == [0xFF, 0xFE] = Enc.decodeUtf16LE . B.drop 2
               | bom == [0xEF, 0xBB, 0xBF]    = Enc.decodeUtf8 . B.drop 3
               | otherwise                    = Enc.decodeUtf8
        decode' :: B.ByteString -> Either Enc.UnicodeException T.Text
        decode' = unsafeDupablePerformIO . try . evaluate . decode -- D:


getSrtBS :: MonadError String m => B.ByteString -> m B.ByteString
getSrtBS bs = do
    arch <- hoistEither . tag "converting to archive" $ Z.toArchiveOrFail bs
    entry <- hoistEither . tag "getting entry" $ getEntry arch
    return $ Z.fromEntry entry


getEntry :: MonadError String m => Z.Archive -> m Z.Entry
getEntry arch = do
    fname <- hoistMaybe "could not find srt" $ fp <|> fp'
    hoistMaybe "this shouldn't happen: no entry found" (Z.findEntryByPath fname arch)
  where fp = find (`endsIn` "eng.srt") (Z.filesInArchive arch)
        fp' = find (`endsIn` ".srt") (Z.filesInArchive arch)


toDiffTime :: SRT.Time -> C.DiffTime
toDiffTime t = C.secondsToDiffTime . toInteger $
      SRT.hour t * 60 * 60 + SRT.minutes t * 60 + SRT.seconds t


endsIn :: String -> String -> Bool
endsIn []     [] = True
endsIn (_:_)  [] = False
endsIn []  (_:_) = False
endsIn (x:xs) (y:ys) = (x == y && endsIn xs ys) || endsIn xs (y:ys)


eitherShow :: Show a => Either a b -> Either String b
eitherShow (Left x) = Left (show x)
eitherShow (Right x) = Right x


tag :: MonadError String m => String -> m a -> m a
tag s m = m `catchError` (\e -> throwError $ "[" ++ s ++ "] " ++ e)


-- Filter out uneeded subtitles ------------------------------------------------

-- | Remove garbage from subs, like non-dialogue and attributions
cleanupSubs :: [Subtitle] -> [Subtitle]
cleanupSubs = filter removeGarbage . map cleanupSub . cleanupFirst


-- | Remove attribution or whatnot if present
cleanupFirst :: [Subtitle] -> [Subtitle]
cleanupFirst = dropWhile isAttribution
    where d sub = TS.toLower $ dialogue sub
          isAttribution sub = any (flip TS.isInfixOf $ d sub) attributionWords
          attributionWords = ["upload", "subtitle", "subscene"]


cleanupSub :: Subtitle -> Subtitle
cleanupSub sub = sub { dialogue = cleanupText . dialogue $ sub }


-- | Remove all <b>, <i>, and <font color=...> tags
cleanupText :: TS.Text -> TS.Text
cleanupText = removeB . removeI . removeFont
    where removeFont _t | TS.null _t = _t
                        | otherwise  = TS.append before $ removeFont' after
              where (before, after) = TS.breakOn "<font color=" _t
          removeFont' = removeFont . TS.drop 7 . snd . TS.breakOn "</font>"
          removeI = TS.replace "</i>" "" . TS.replace "<i>" ""
          removeB = TS.replace "</b>" "" . TS.replace "<b>" ""


removeGarbage :: Subtitle -> Bool
removeGarbage = not . TS.null . TS.strip . dialogue
