module SrtExtractSpec (main, spec) where

import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.ByteString.Lazy.Char8 as BSC
import Test.Hspec
import SrtExtract

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseSrtFromZip" $ do
    it "returns the subs when correct" $ do
      let subs = case parseSrtFromZip zipWithSubs of
                   Left s -> error $ "could not parse srt!: " ++ s
                   Right subs -> subs
      (length subs) `shouldBe` 1

    it "returns Left when no SRT exists" $ do
      (parseSrtFromZip zipWithNoSubs) `shouldBe` (Left "[getting entry] could not find srt")


zipWithNoSubs = decodeB64 "\
\UEsDBAoAAAAAAGmeaUhEV9WnDQAAAA0AAAAKAAAAYnV0d2h5LnR4dHdoYXRpc2dvaW5nb25QSwME\
\CgAAAAAAhp5pSAAAAAAAAAAAAAAAAAcAAAB3YXQudHh0UEsBAj8ACgAAAAAAaZ5pSERX1acNAAAA\
\DQAAAAoAJAAAAAAAAAAgAAAAAAAAAGJ1dHdoeS50eHQKACAAAAAAAAEAGACWBV/0ZnrRAbpli8ZN\
\etEBumWLxk160QFQSwECPwAKAAAAAACGnmlIAAAAAAAAAAAAAAAABwAkAAAAAAAAACAAAAA1AAAA\
\d2F0LnR4dAoAIAAAAAAAAQAYAAqnjBRnetEBCqeMFGd60QEKp4wUZ3rRAVBLBQYAAAAAAgACALUA\
\AABaAAAACABTdWJzY2VuZQ=="


zipWithSubs = decodeB64 "\
\UEsDBAoAAAAAAGmGaUiv8bWsCAAAAAgAAAALAAAAbm90c3Vicy50eHRub3RzdWJzIVBLAwQKAAAA\
\AAAxhmlIqgdL2T4AAAA+AAAACAAAAHN1YnMuc3J077u/MQ0KMDA6MDA6MDEsNzgxIC0tPiAwMDow\
\MDowMywyMTUNCmZvb2Jhcg0KYmF6IGRpbmcgZG9uZw0KDQpQSwECPwAKAAAAAABphmlIr/G1rAgA\
\AAAIAAAACwAkAAAAAAAAACAAAAAAAAAAbm90c3Vicy50eHQKACAAAAAAAAEAGACpWg/PTXrRAbpl\
\i8ZNetEBumWLxk160QFQSwECPwAKAAAAAAAxhmlIqgdL2T4AAAA+AAAACAAkAAAAAAAAACAAAAAx\
\AAAAc3Vicy5zcnQKACAAAAAAAAEAGABOY9GQTXrRAX/aentNetEBf9p6e0160QFQSwUGAAAAAAIA\
\AgC3AAAAlQAAAAgAU3Vic2NlbmU="


decodeB64 :: String -> BSC.ByteString
decodeB64 s = case (B64.decode . BSC.pack $ s) of
                  Left _ -> error "could not parse zip"
                  Right bs -> bs
