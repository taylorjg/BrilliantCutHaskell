import BrilliantCut
import qualified Data.ByteString.Lazy.Char8 as C8
import Test.Hspec

input1 = "test/input1.json"
input2 = "test/input2.json"
input3 = "test/input3.json"
input4 = "test/input4.json"

main :: IO ()
main = hspec $ do
  describe "BrilliantCut tests" $ do

    describe "given example" $ do
      it "largest profit" $ do
        bs <- C8.readFile input1
        largestProfitByteString bs `shouldBe` Just 27

    describe "given example x 2 raw chunks" $ do
      it "largest profit" $ do
        bs <- C8.readFile input2
        largestProfitByteString bs `shouldBe` Just (27 * 2)

    describe "given example x 2 gem types" $ do
      it "largest profit" $ do
        bs <- C8.readFile input3
        largestProfitByteString bs `shouldBe` Just (27 * 2)

    describe "given example x 2 gem types x 2 raw chunks" $ do
      it "largest profit" $ do
        bs <- C8.readFile input4
        largestProfitByteString bs `shouldBe` Just (27 * 2 * 2)
