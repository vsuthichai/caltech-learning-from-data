import scala.util.Random

object Perceptron {
  private class Point {
    val x: Double = randomDouble
    val y: Double = randomDouble
    override def toString: String = "1 " + x + " " + y
  }

  private val R = new Random             // random generator
  private var n: Int = _                 // training set size
  private var x: Array[Point] = _        // dataset
  private var w: Array[BigDecimal] = _   // weight vector

  private val f = new Array[Point](2)    // target function
  private var f_slope: Double = _        // m in (y = mx + b)
  private var f_intercept: Double = _    // y in (y = mx + b)
  private var f_x: Array[BigDecimal] = _ // f(x)

  private var g_slope: Double = _        // m in (y = mx + b)
  private var g_intercept: Double = _    // y in (y = mx + b)
  private var g_x: Array[BigDecimal] = _ // g(x)

  private def randomDouble() = R.nextDouble * (if (R.nextBoolean) 1 else -1)
  private def slope(p: Array[Point]) = (p(1).y - p(0).y) / (p(1).x - p(0).x)
  private def intercept(p: Array[Point]) = p(1).y - (slope(p) * p(1).x)

  // true: 1, false: -1
  private def sign(m: Double, b: Double, p: Point) = if (p.y > m * p.x + b) 1 else -1
  private def sign2int(bool: Boolean) = if (bool) 1 else -1

/*
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
*/

  private def linearRegression {
    var X_pi = new Array[Array[BigDecimal]](3)
    val identity = new Array[Array[BigDecimal]](3)

    // (X transposed * X) -- create identity matrix
    for (row <- 0 until 3) {
      identity(row) = new Array[BigDecimal](3)
      X_pi(row) = new Array[BigDecimal](3)
      for (col <- 0 until 3) {
        identity(row)(col) = BigDecimal(if (row == col) 1.0 else 0.0)
        var row_col = BigDecimal(0.0)
        for (i <- 0 until n) {
          var a: Double = 0.0
          var b: Double = 0.0
          if (row == 0) a = 1.0 else if (row == 1) a = x(i).x else a = x(i).y
          if (col == 0) b = 1.0 else if (col == 1) b = x(i).x else b = x(i).y
          row_col += (BigDecimal(a) * BigDecimal(b))
        }
        X_pi(row)(col) = row_col
      }
    }

    // (X transposed * X)^-1 -- inversion
    for (col <- 0 until 3) {
      val divider = X_pi(col)(col)
      for (col1 <- 0 until 3) {
        X_pi(col)(col1) = X_pi(col)(col1) / divider
        identity(col)(col1) = identity(col)(col1) / divider
      }
      for (row <- 0 until col) {
        val mult = X_pi(row)(col)
        for (col1 <- 0 until 3) {
          X_pi(row)(col1) = X_pi(row)(col1) - X_pi(col)(col1) * mult
          identity(row)(col1) = identity(row)(col1) - identity(col)(col1) * mult
        }
      }
      for (row <- col + 1 until 3) {
        val mult = X_pi(row)(col)
        for (col1 <- 0 until 3) {
          X_pi(row)(col1) = X_pi(row)(col1) - X_pi(col)(col1) * mult
          identity(row)(col1) = identity(row)(col1) - identity(col)(col1) * mult
        }
      }
    }

    // multiply transpose of X
    X_pi = new Array[Array[BigDecimal]](3)
    for (row <- 0 until 3) {
      X_pi(row) = new Array[BigDecimal](n)
      for (col <- 0 until n) {
        X_pi(row)(col) = identity(row)(0) * 1 + identity(row)(1) * BigDecimal(x(col).x) + identity(row)(2) * BigDecimal(x(col).y)
      }
    }
    //printMatrix(X_pi)

    // multiply y vector
    for (row <- 0 until 3) {
      var row_col = BigDecimal(0.0)
      for (col <- 0 until n) {
        row_col += X_pi(row)(col) * f_x(col)
      }
      w(row) = row_col
    }
  }

  private def printMatrix(m: Array[Array[BigDecimal]]) {
    for (i <- 0 until m.length) {
      for (j <- 0 until m(i).length) {
        print(m(i)(j) + " ")
      }
      println()
    }
    println()
  }

  private def printPoints {
    for (i <- 0 until x.length) {
      println(x(i))
    }
    println()
  }

  private def printWeights {
    for (i <- 0 until w.length) {
      print(w(i) + " ")
    }
    println()
  }

  def experiment(N: Int, runs: Int) {
    n = N
    var EinAvg = 0.0
    var EoutAvg = 0.0
    var EinAvg_t = 0.0
    for (i <- 0 until runs) {
      w = new Array[BigDecimal](3)
      f(0) = new Point
      f(1) = new Point
      f_slope = slope(f) 
      f_intercept = intercept(f)

      x = new Array[Point](n)
      f_x = new Array[BigDecimal](n)
      g_x = new Array[BigDecimal](n)

      for (j <- 0 until n) {
        x(j) = new Point
	f_x(j) = BigDecimal(sign(f_slope, f_intercept, x(j)))
      }

      linearRegression
      //printWeights

      var Ein = 0
      for (j <- 0 until n) {
        g_x(j) = w(0) + w(1) * BigDecimal(x(j).x) + w(2) * BigDecimal(x(j).y)
        if ((f_x(j) > BigDecimal(0) && g_x(j) < BigDecimal(0)) || (f_x(j) < BigDecimal(0) && g_x(j) > BigDecimal(0))) {
          Ein += 1
        }
      }
      EinAvg += (Ein.toDouble / n)

      var Eout = 0

      x = new Array[Point](1000)
      f_x = new Array[BigDecimal](1000)
      g_x = new Array[BigDecimal](1000)

      for (j <- 0 until 1000) {
        x(j) = new Point
	f_x(j) = BigDecimal(sign(f_slope, f_intercept, x(j)))
        g_x(j) = w(0) + w(1) * BigDecimal(x(j).x) + w(2) * BigDecimal(x(j).y)
        if ((f_x(j) > BigDecimal(0) && g_x(j) < BigDecimal(0)) || (f_x(j) < BigDecimal(0) && g_x(j) > BigDecimal(0))) {
          Eout += 1
        }
      }
      EoutAvg += (Eout.toDouble / 1000)

      for (j <- 0 until 1000) {
        val p = new Point
        f_x(j) = BigDecimal(p.x * p.x + p.y * p.y - 0.6)
        if (R.nextDouble < 0.1) 
          f_x(j) = f_x(j) * BigDecimal(-1.0)
      }

      linearRegression

      var Ein_t = 0
      for (j <- 0 until 1000) {
        g_x(j) = w(0) + w(1) * BigDecimal(x(j).x) + w(2) * BigDecimal(x(j).y)
        if ((f_x(j) > BigDecimal(0) && g_x(j) < BigDecimal(0)) || (f_x(j) < BigDecimal(0) && g_x(j) > BigDecimal(0))) {
          Ein_t += 1
        }
      }
      EinAvg_t += (Ein_t.toDouble / 1000)
    }
    println("Ein = " + (EinAvg / runs))     // ~0.038
    println("Eout = " + (EoutAvg / runs))   // ~0.047
    println("Ein_noise = " + (EinAvg_t / runs)) // ~0.48
  }

  def main(args: Array[String]) {
    //this.experiment(10, 1000)
    this.experiment(100, 1000)
  }
}

