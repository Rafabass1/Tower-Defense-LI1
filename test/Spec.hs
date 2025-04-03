module Main (main) where
import Test.HUnit
import Tarefa1Spec
import Tarefa2Spec
import Tarefa3Spec

testSuite :: Test
testSuite =
  TestLabel "Spec Test Suit" $
    test
      [ testesTarefa1,
        testesTarefa2,
        testesTarefa3
      ]

main :: IO ()
main = runTestTTAndExit $ test [testSuite]