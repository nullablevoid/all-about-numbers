import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.OptionValues

import scala.language.postfixOps


class NumberOfDigitsSpec extends AnyFlatSpec
  with Matchers
  with OptionValues {

  import NumberOfDigits.getNumberOfIntegersDigits

  "Zero" should "have a digit" in {
    getNumberOfIntegersDigits(0).value shouldEqual 1
  }

  "various integers" should "have the right numbers of digits" in {
    getNumberOfIntegersDigits(-1).value shouldEqual 1
    getNumberOfIntegersDigits(1).value shouldEqual 1
    getNumberOfIntegersDigits(-1001).value shouldEqual 4
    getNumberOfIntegersDigits(1001).value shouldEqual 4
  }

  "different bigInts" should "have the right number of digits" in {
    getNumberOfIntegersDigits(BigDecimal("1251523523532532353223")).value shouldEqual 22
    getNumberOfIntegersDigits(BigDecimal("-1251523523532532353223")).value shouldEqual 22

    getNumberOfIntegersDigits(BigDecimal("9876543211251523523532532353223")).value shouldEqual 31
    getNumberOfIntegersDigits(BigDecimal("-9876543211251523523532532353223")).value shouldEqual 31

    getNumberOfIntegersDigits(BigDecimal("98765432112515235235325323532239876543211251523523532532353223")).value shouldEqual 62
    getNumberOfIntegersDigits(BigDecimal("-98765432112515235235325323532239876543211251523523532532353223")).value shouldEqual 62


    getNumberOfIntegersDigits(BigDecimal("9876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223")).value shouldEqual 124
    getNumberOfIntegersDigits(BigDecimal("-9876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223")).value shouldEqual 124


    getNumberOfIntegersDigits(BigDecimal("98765432112515235235325323532239876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223")).value shouldEqual 248
    getNumberOfIntegersDigits(BigDecimal("-98765432112515235235325323532239876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223")).value shouldEqual 248

    getNumberOfIntegersDigits(BigDecimal("-98765432112515235235325323532239876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223")).value shouldEqual 248
    getNumberOfIntegersDigits(BigDecimal("98765432112515235235325323532239876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223")).value shouldEqual 248
  }

  "different bigDecimals" should "have the right number of digits" in {
    getNumberOfIntegersDigits(0.0000000000000000000000000000000000000004).value shouldEqual 1
    getNumberOfIntegersDigits(0.0000000000000000000000000000000000000004).value shouldEqual 1

    getNumberOfIntegersDigits(BigDecimal("-1251523523532532353223.6523125230000000000")).value shouldEqual 22
    getNumberOfIntegersDigits(BigDecimal("1251523523532532353223.6523125230000000000")).value shouldEqual 22


    getNumberOfIntegersDigits(BigDecimal("9876543211251523523532532353223.226523125230000000000")).value shouldEqual 31
    getNumberOfIntegersDigits(BigDecimal("9876543211251523523532532353223.22652312523000000000022652312523")).value shouldEqual 31


    getNumberOfIntegersDigits(BigDecimal("98765432112515235235325323532239876543211251523523532532353223.22652312523000000000022652312523")).value shouldEqual 62

    getNumberOfIntegersDigits(BigDecimal("9876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223.22652312523000000000022652312523")).value shouldEqual 124

    getNumberOfIntegersDigits(BigDecimal("98765432112515235235325323532239876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223.22652312523000000000022652312523")).value shouldEqual 248
    getNumberOfIntegersDigits(BigDecimal("98765432112515235235325323532239876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223987654321125152352353253235322398765432112515235235325323532239876543211251523523532532353223.22652312523000000000022652312523111")).value shouldEqual 248
  }

  "different implementations for Number interface" should "have the right number of digits" in {
    getNumberOfIntegersDigits(100).value shouldEqual (3)
    getNumberOfIntegersDigits(1234567890987654321L).value shouldEqual 19
    getNumberOfIntegersDigits(45.52333333333333333333333333333333D).value shouldEqual 2
    getNumberOfIntegersDigits(Long.MaxValue).value shouldEqual 19
    getNumberOfIntegersDigits(Long.MaxValue.toFloat).value shouldEqual 19
    getNumberOfIntegersDigits(Long.MaxValue.toDouble).value shouldEqual 19


    getNumberOfIntegersDigits(100 byteValue).value shouldEqual 3
    getNumberOfIntegersDigits(100 shortValue).value shouldEqual 3
  }

  "different potential precision missing at the boundaries" should "have the right number of digits" in {
    getNumberOfIntegersDigits(9.9999999999999D).value shouldEqual 1
    getNumberOfIntegersDigits(99.9999999999999D).value shouldEqual 2

    getNumberOfIntegersDigits(9.99999f).value shouldEqual 1
  }

  "When a null is passed, the function" should "return None" in {
    getNumberOfIntegersDigits(null) shouldEqual None
  }

  "When a number outside the precision of its type is given, the function" should "return the number of digits for the representation" in {
    val numberWithLostPrecision: Float = 99.99999999999999999999999999999999999999999999999999999f

    // Because numberWithLostPrecision is internally already represented as 100.0f, the result will be 2.
    // From #getNumberOfIntegersDigits perspective, this is right.
    getNumberOfIntegersDigits(numberWithLostPrecision) shouldEqual getNumberOfIntegersDigits(100)
  }
}
