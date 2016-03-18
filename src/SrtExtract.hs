module SrtExtract (
  parseSrtFromZip
)
where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as Enc
import qualified Data.ByteString.Lazy as B
import qualified Codec.Archive.Zip as Z
import qualified Text.Subtitles.SRT as SRT

import Data.List (find)
import Data.Char (isSpace)
import Data.Attoparsec.Text (parseOnly, maybeResult)


parseSrtFromZip :: B.ByteString -> Either String SRT.Subtitles
parseSrtFromZip zipbs = getSrtBS zipbs >>= bsToSubs


bsToSubs :: B.ByteString -> Either String SRT.Subtitles
bsToSubs bs = do
    t <- eitherShow $ Enc.decodeUtf8' bs
    parseOnly SRT.parseSRT $ T.toStrict t


getSrtBS :: B.ByteString -> Either String B.ByteString
getSrtBS bs = do
    arch <- Z.toArchiveOrFail bs
    entry <- getEntry arch
    return $ B.drop 3 $ Z.fromEntry entry


getEntry :: Z.Archive -> Either String Z.Entry
getEntry arch = do
    fp <- fromMaybe "could not find srt" $ find (\s -> endsIn s ".srt") (Z.filesInArchive arch)
    fromMaybe "wat?" (Z.findEntryByPath fp arch)


endsIn :: String -> String -> Bool
endsIn []     [] = True
endsIn (x:_)  [] = False
endsIn []  (x:_) = False
endsIn (x:xs) (y:ys) = (x == y && endsIn xs ys) || endsIn xs (y:ys)


fromMaybe :: String -> Maybe b -> Either String b
fromMaybe s Nothing = Left s
fromMaybe _ (Just x) = Right x


eitherShow :: Show a => Either a b -> Either String b
eitherShow (Left x) = Left (show x)
eitherShow (Right x) = Right x
