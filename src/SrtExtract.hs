module SrtExtract (
  parseSrtFromZip
)
where

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as Enc
import qualified Data.Text.Encoding.Error as Enc
import qualified Data.ByteString.Lazy as B
import qualified Codec.Archive.Zip as Z
import qualified Text.Subtitles.SRT as SRT

import Data.List (find)
import Data.Attoparsec.Text (parseOnly)
import Control.Exception (try, evaluate)
import System.IO.Unsafe (unsafeDupablePerformIO)


parseSrtFromZip :: B.ByteString -> Either String SRT.Subtitles
parseSrtFromZip zipbs = getSrtBS zipbs >>= bsToSubs


bsToSubs :: B.ByteString -> Either String SRT.Subtitles
bsToSubs bs = do
    t <- tag "decoding" . eitherShow . decode' $ bs
    tag "using srt parser" . parseOnly SRT.parseSRT . T.toStrict . T.strip $ t
  where bom = B.unpack $ B.take 3 bs
        decode | (take 2 bom) == [0xFE, 0xFF] = Enc.decodeUtf16BE . B.drop 2
               | (take 2 bom) == [0xFF, 0xFE] = Enc.decodeUtf16LE . B.drop 2
               | bom == [0xEF, 0xBB, 0xBF]    = Enc.decodeUtf8 . B.drop 3
               | otherwise                    = Enc.decodeUtf8
        decode' :: B.ByteString -> Either Enc.UnicodeException T.Text
        decode' = unsafeDupablePerformIO . try . evaluate . decode -- D:


getSrtBS :: B.ByteString -> Either String B.ByteString
getSrtBS bs = do
    arch <- tag "converting to archive" $ Z.toArchiveOrFail bs
    entry <- tag "getting entry" $ getEntry arch
    return $ Z.fromEntry entry


getEntry :: Z.Archive -> Either String Z.Entry
getEntry arch = do
    fp <- fromMaybe "could not find srt" $ find (\s -> endsIn s ".srt") (Z.filesInArchive arch)
    fromMaybe "this shouldn't happen: no entry found" (Z.findEntryByPath fp arch)


endsIn :: String -> String -> Bool
endsIn []     [] = True
endsIn (_:_)  [] = False
endsIn []  (_:_) = False
endsIn (x:xs) (y:ys) = (x == y && endsIn xs ys) || endsIn xs (y:ys)


fromMaybe :: String -> Maybe b -> Either String b
fromMaybe s Nothing = Left s
fromMaybe _ (Just x) = Right x


eitherShow :: Show a => Either a b -> Either String b
eitherShow (Left x) = Left (show x)
eitherShow (Right x) = Right x


tag :: String -> Either String a -> Either String a
tag _ (Right x) = Right x
tag t (Left s) = Left $ "[" ++ t ++ "] " ++ s
