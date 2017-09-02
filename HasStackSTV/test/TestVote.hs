module TestVote (voteTests) where

import Test.HUnit
import Vote

voteTests :: Test
voteTests = TestLabel "Vote Tests" $
    TestList [
      basicVote
      , multiVote
      , basicVoteFail
      , incompleteVote
      , preferenceConstruction
    ]

basicVote =
    TestCase $ assertEqual
    "Should Parse simple Vote"
    (Vote (checkedPreference 1 2 3) 1)
    (fromString "1 1 2 3")

multiVote =
    TestCase $ assertEqual
    "Should Parse multi-vote configuration"
    (Vote (checkedPreference 2 3 5) 2)
    (fromString "2 2 3 5")

basicVoteFail =
    TestCase $ assertEqual
    "Should not parse incorrect vote"
    (Vote (checkedPreference 0 0 0) 0)
    (fromString "asd")

incompleteVote =
    TestCase $ assertEqual
    "Should parse incomplete vote"
    (Vote (checkedPreference 3 2 0) 4)
    (fromString "4 3 2")

preferenceConstruction =
    TestLabel "Preference constructors" $ TestList [
        TestCase $ assertEqual
            "first missing"
            (checkedPreference 1 2 0)
            (checkedPreference 0 1 2),
        TestCase $ assertEqual
            "second missing"
            (checkedPreference 1 2 0)
            (checkedPreference 1 0 2),
        TestCase $ assertEqual
            "first and second missing"
            (checkedPreference 1 0 0)
            (checkedPreference 0 0 1),
        TestCase $ assertEqual
            "first malformed"
            (checkedPreference 1 2 0)
            (checkedPreference (-1) 1 2),
        TestCase $ assertEqual
            "second malformed"
            (checkedPreference 1 2 0)
            (checkedPreference 1 (-1) 2),
        TestCase $ assertEqual
            "third malformed"
            (checkedPreference 1 2 0)
            (checkedPreference 1 2 (-1)),
        TestCase $ assertEqual
            "first and second malformed"
            (checkedPreference 1 0 0)
            (checkedPreference (-1) (-2) 1),
        TestCase $ assertEqual
            "first and third malformed"
            (checkedPreference 1 0 0)
            (checkedPreference (-1) 1 (-2)),
        TestCase $ assertEqual
            "second and third malformed"
            (checkedPreference 1 0 0)
            (checkedPreference 1 (-1) (-2))
        ]
