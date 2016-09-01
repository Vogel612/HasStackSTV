import Test.HUnit
import Vote

main :: IO()
main = defaultMainWithOpts
    [testCase "BasicVote" basicVote,
    testCase "BasicVoteFail" basicVoteFail]
    mempty

basicVote :: Test
basicVote =
    TestCase $ assertEqual "Should Parse simple Vote" (Vote (Preference 1 2 3) 1) (fromString "1 1 2 3")
    TestCase $ assertEqual "Should Parse multi-vote configuration" (Vote (Preference 2 3 5) 2) (fromString "2 2 3 5")

basicVoteFail :: Test
basicVote =
    TestCase $ assertEqual "Should not parse incorrect preference" (Vote None 1) (fromString "1 1 2")
    TestCase $ assertEqual "Should not parse incorrect vote" (Vote None 0) (fromString "asd")
