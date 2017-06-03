{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Acronym (abbreviate, shouldSplit, SplitVerdict(..), revrev, doSplit', doSplit, notEmpty)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs =
    describe "tests" $ do
      describe "subtests" $ do
        it "identifies chars properly" $ do
          shouldSplit ' ' `shouldBe` Discard
          shouldSplit '-' `shouldBe` Discard
          shouldSplit 'T' `shouldBe` Split
          shouldSplit 's' `shouldBe` Gather

        it "reverses nested list" $ do
          revrev [[1, 2], [2, 3]] `shouldBe` [[3, 2], [2, 1]]

        it "does intermediates" $ do
          doSplit' shouldSplit [] [] "ab" `shouldBe` ["ba"]
          doSplit' shouldSplit [] [] "ab " `shouldBe` ["ba"]
          doSplit' shouldSplit [] [] "ab bc" `shouldBe` ["cb", "ba"]
          doSplit' shouldSplit [] [] "HTT" `shouldBe` ["TTH", ""]
          doSplit' shouldSplit [] [] "HTT XAB" `shouldBe` ["BAX", "", "", "TTH", ""]
          doSplit' shouldSplit [] [] "Hyper" `shouldBe` ["repyH", ""]
        it "camels" $ do
          doSplit' shouldSplit [] [] "HyT" `shouldBe` ["T", "yH", ""]
          doSplit' shouldSplit [] [] "HyperText" `shouldBe` ["txeT", "repyH", ""]

        it "splits" $ do
          (filter notEmpty $ doSplit' shouldSplit [] [] "ab bc") `shouldBe` ["cb", "ba"]
          (revrev $ filter notEmpty $ doSplit' shouldSplit [] [] "ab bc") `shouldBe` ["ab", "bc"]
          doSplit shouldSplit "ab bc" `shouldBe` ["ab", "bc"]

      let test Case {..} = it description $ abbreviate input `shouldBe` expected
      describe "abbreviate" $ for_ cases test

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: String
                 }

cases :: [Case]
cases = [ Case { description = "basic"
               , input       = "Portable Network Graphics"
               , expected    = "PNG"
               }
        , Case { description = "lowercase words"
               , input       = "Ruby on Rails"
               , expected    = "ROR"
               }
        , Case { description = "camelcase"
               , input       = "HyperText Markup Language"
               , expected    = "HTML"
               }
        , Case { description = "punctuation"
               , input       = "First In, First Out"
               , expected    = "FIFO"
               }
        , Case { description = "all caps words"
               , input       = "PHP: Hypertext Preprocessor"
               , expected    = "PHP"
               }
        , Case { description = "non-acronym all caps word"
               , input       = "GNU Image Manipulation Program"
               , expected    = "GIMP"
               }
        , Case { description = "hyphenated"
               , input       = "Complementary metal-oxide semiconductor"
               , expected    = "CMOS"
               }
        ]
