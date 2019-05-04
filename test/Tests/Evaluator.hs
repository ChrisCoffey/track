module Tests.Evaluator (
    tests
) where

import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Evaluation" [
     trackingTests
    ,cleanupTests
    ,categoryTests
    ,reportTests
    ]

trackingTests :: TestTree
trackingTests = testGroup "tracking" [
     testProperty "start must always come before stop or switch" $ undefined
    ,testProperty "providing a category is optional" $ undefined
    ,testProperty "switch == stop . start" $ undefined
    ,testProperty "stop . stop is a noop" $ undefined
    ,testProperty "start . start is a noop" $ undefined
    ]


cleanupTests :: TestTree
cleanupTests = testGroup "log cleanup" [
     testProperty "edit details can only change the details of the head element" $ undefined
    ,testProperty "edit cannot change a pending element" $ undefined
    ,testProperty "DeleteByCategory removes all elements of that category, but no others" $ undefined
    ,testProperty "DeleteByTime removes everything in the specified window, but no others" $ undefined
    ]

categoryTests :: TestTree
categoryTests = testGroup "category management" [
     testProperty "Can define new categories, no duplicates" $ undefined
    ,testProperty "Listing categories shows all defined categories" $ undefined
    ,testProperty "ChangeCategoryName changes name & rewrites log entries" $ undefined
    ]

reportTests :: TestTree
reportTests = testGroup "reports" [
     testProperty "category report accumulates all time" $ undefined
    ,testProperty "time of day report accumulates all categories" $ undefined
    ]
