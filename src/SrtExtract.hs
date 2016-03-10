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


parseSrtFromZip :: B.ByteString -> Maybe SRT.Subtitles
parseSrtFromZip zipbs = getSrtBS zipbs >>= bsToMaybeSubs


bsToMaybeSubs :: B.ByteString -> Maybe SRT.Subtitles
bsToMaybeSubs bs = do
    t <- text
    case parseOnly SRT.parseSRT t of
          Left _  -> Nothing
          Right s -> Just s
    where text = case Enc.decodeUtf8' bs of
                   Left  _ -> Nothing
                   Right t -> Just $ T.toStrict t


getSrtBS :: B.ByteString -> Maybe B.ByteString
getSrtBS bs = do
    arch <- toArchive bs
    entry <- getEntry arch
    return $ B.drop 3 $ Z.fromEntry entry


toArchive :: B.ByteString -> Maybe Z.Archive
toArchive bs = case Z.toArchiveOrFail bs of
        Left _  -> Nothing
        Right a -> Just a


getEntry :: Z.Archive -> Maybe Z.Entry
getEntry arch = do
    fp <- find (\s -> endsIn s ".srt") (Z.filesInArchive arch)
    Z.findEntryByPath fp arch


endsIn :: String -> String -> Bool
endsIn []     [] = True
endsIn (x:_)  [] = False
endsIn []  (x:_) = False
endsIn (x:xs) (y:ys) = (x == y && endsIn xs ys) || endsIn xs (y:ys)
