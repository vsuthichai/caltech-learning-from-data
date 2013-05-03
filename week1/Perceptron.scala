import scala.util.Random

object Perceptron {
  private class Point {
    val x: Double = randomDouble
    val y: Double = randomDouble
    override def toString: String = "[" + x + ", " + y + "]"
  }

  private val R = new Random             // random generator
  private var n: Int = _                 // training set size
  private var x: Array[Point] = _        // dataset
  private var w: Array[Double] = _       // weight vector

  private val f = new Array[Point](2)    // target function
  private var f_slope: Double = _        // m in (y = mx + b)
  private var f_intercept: Double = _    // y in (y = mx + b)
  private var f_x: Array[Boolean] = _    // f(x)

  private val g = new Array[Point](2)    // hypothesis function
  private var g_slope: Double = _        // m in (y = mx + b)
  private var g_intercept: Double = _    // y in (y = mx + b)
  private var g_x: Array[Boolean] = _    // g(x)

  private def randomDouble() = R.nextDouble * (if (R.nextBoolean) 1 else -1)
  private def slope(p: Array[Point]) = (p(1).y - p(0).y) / (p(1).x - p(0).x)
  private def intercept(p: Array[Point]) = p(1).y - (slope(p) * p(1).x)

  // true: 1, false: -1
  private def sign(m: Double, b: Double, p: Point) = p.y > m * p.x + b
  private def sign2int(bool: Boolean) = if (bool) 1 else -1
  private def isClassified(i: Int): Boolean = f_x(i) == g_x(i)
  private def allClassified: Boolean = {
    for (i <- 0 until n) {
      if (!isClassified(i))
        return false
    }
    return true
  }

  private def calc_g_x(p: Point): Boolean =
    (w(0) * 1) + (w(1) * p.x) + (w(2) * p.y) > 0

  private def classifyPoint(i: Int) {
    w(0) += (1 * sign2int(f_x(i)))
    w(1) += (x(i).x * sign2int(f_x(i)))
    w(2) += (x(i).y * sign2int(f_x(i)))
    for (j <- 0 until n)
      g_x(j) = calc_g_x(x(j))
  }

  def experiment(N: Int, runs: Int) {
    n = N
    var it = 0
    var errs = 0
    for (i <- 0 until runs) {
      w = Array(0.0, 0.0, 0.0)
      f(0) = new Point
      f(1) = new Point
      f_slope = slope(f) 
      f_intercept = intercept(f)

      g(0) = new Point
      g(1) = new Point
      g_slope = slope(g)
      g_intercept = intercept(g)

      x = new Array[Point](n)
      f_x = new Array[Boolean](n)
      g_x = new Array[Boolean](n)

      for (j <- 0 until n) {
        x(j) = new Point
	f_x(j) = sign(f_slope, f_intercept, x(j))
	g_x(j) = sign(g_slope, g_intercept, x(j))
      }

      while (!allClassified) {
        var i = R.nextInt(n)
        while (isClassified(i))
	  i = R.nextInt(n)
        it += 1
	classifyPoint(i)
      }

      for (j <- 0 until 10000) {
        val p = new Point
        if (sign(f_slope, f_intercept, p) != calc_g_x(p))
          errs += 1
      }
    }
    println(it.toDouble / runs)
    println(errs.toDouble / 10000 / runs)
  }

  def main(args: Array[String]) {
    this.experiment(10, 1000)
    this.experiment(100, 1000)
  }
}
