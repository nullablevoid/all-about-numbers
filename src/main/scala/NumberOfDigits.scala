import java.text.DecimalFormat
import java.util.concurrent.atomic.AtomicInteger
import scala.math.BigDecimal.RoundingMode

object NumberOfDigits extends App {
  def getNumberOfIntegersDigits(number: Number): Option[Int] = {

    if number == null then return None

    val bigDecimalValue: BigDecimal = BigDecimal(number.toString) // It might loose some precision when doing #toString
    // Get rid of the sign and the fractional part
    val numberBigInteger: BigInt = bigDecimalValue.abs.toBigInt

    // the precision of an integer is actual its length
    Some(BigDecimal(numberBigInteger).precision)
  }
}
