import BrilliantCut
import qualified Data.ByteString.Lazy.Char8 as BS
import Test.HUnit
import System.Exit (exitFailure)

input1 = "test/input1.json"
input2 = "test/input2.json"
input3 = "test/input3.json"
input4 = "test/input4.json"

testCommon :: String -> Int -> IO ()
testCommon fn expected = do
    bs <- BS.readFile fn
    let maybeAnswer = largestProfitByteString bs
    assertEqual "expected answer to be correct" (Just expected) maybeAnswer

test1 = TestCase $ do
    testCommon input1 27

test2 = TestCase $ do
    testCommon input2 (27 * 2)

test3 = TestCase $ do
    testCommon input3 (27 * 2)

test4 = TestCase $ do
    testCommon input4 (27 * 2 * 2)

tests = TestList [
    TestLabel "given example" test1,
    TestLabel "given example x 2 raw chunks" test2,
    TestLabel "given example x 2 gem types" test3,
    TestLabel "given example x 2 gem types x 2 raw chunks" test4
    ]

main :: IO ()
main = do
    counts <- runTestTT tests
    if errors counts > 0 || failures counts > 0
        then exitFailure
        else return ()
