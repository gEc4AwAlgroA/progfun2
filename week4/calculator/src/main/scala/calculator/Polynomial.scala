package calculator
import scala.math

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(delta() match {
      case x if x >= 0 => Set((-b() + math.sqrt(x)) / (2 * a()), (-b() - math.sqrt(x)) / (2 * a()))
      case _ => Set()
    })
  }
}
