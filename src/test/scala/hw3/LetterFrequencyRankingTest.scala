package hw3

import hw3.Main.letterFrequencyRanking
import org.scalatest.{FunSuite, Matchers}

class LetterFrequencyRankingTest extends FunSuite with Matchers {
  test("Simple")          { letterFrequencyRanking("hello") shouldBe "leho" }
  test("Capital letters") { letterFrequencyRanking("AaaAaaAaa") shouldBe "a" }
  test("Punctuation")     { letterFrequencyRanking("Sic!") shouldBe "cis" }
  test("No Letters")     { letterFrequencyRanking("@]{[<>[÷[×°˘˛ˇ^˘~~]$[!") shouldBe "" }
  test("White spaces")     { letterFrequencyRanking("Hey, look at this tabulator '\t'!") shouldBe "taolhesyuibrk" }
  test("Digits")     { letterFrequencyRanking("R2-D2") shouldBe "dr" }
  test("Diacritics")     { letterFrequencyRanking("Alžběta Přemyslovna") shouldBe "alesnytmvžbřpěo" }
}