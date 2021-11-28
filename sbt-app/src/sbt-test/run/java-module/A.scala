import java.sql.Date
import java.time.LocalDate

object Test {
  def main(argv: Array[String]): Unit = {
    val now = Date.valueOf(LocalDate.now())
  }
}
