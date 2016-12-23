package calculator

object Polynomial {

  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] =
    Signal(b.apply() * b.apply() - 4 * a.apply() * c.apply())

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal(
    if (delta.apply() < 0) Set()
    else Set(solution(a, b, Math.sqrt(delta.apply())), solution(a, b, -Math.sqrt(delta.apply())))
  )

  def solution(a: Signal[Double], b: Signal[Double], deltaSqrt: Double): Double = (-b.apply + deltaSqrt) / 2 * a.apply

}
