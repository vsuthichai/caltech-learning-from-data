import scala.util.Random

object Perceptron2 {
  private class Point(size: Int) {
    var x: Array[BigDecimal] = new Array[BigDecimal](size)
    x(0) = 1
    x(1) = BigDecimal(R.nextDouble * (if (R.nextBoolean) 1 else -1))
    x(2) = BigDecimal(R.nextDouble * (if (R.nextBoolean) 1 else -1))
    def transform(size: Int) {
      val new_x = new Array[BigDecimal](size)
      for (i <- 0 until x.length) {
        new_x(i) = x(i)
      }
      x = new_x
    }
    override def toString: String = {
      var str = ""
      for (i <- 0 until x.length) {
        str += " " + x(i)
      }
      //str += "\n"
      str
    }
  }

  private val R = new Random              // random generator
  private var n: Int = _                  // training set size
  private var x: Array[Point] = _         // dataset
  private var w: Array[BigDecimal] = _    // weight vector

  private val f = new Array[Point](2)     // target function
  private var f_slope: BigDecimal = _     // m in (y = mx + b)
  private var f_intercept: BigDecimal = _ // y in (y = mx + b)
  private var f_x: Array[BigDecimal] = _  // f(x)

  private var g_x: Array[BigDecimal] = _  // g(x)

  private def slope(p: Array[Point]): BigDecimal = (p(1).x(2) - p(0).x(2)) / (p(1).x(1) - p(0).x(1))
  private def intercept(p: Array[Point]): BigDecimal = (p(1).x(2)) - (slope(p) * p(1).x(1))

  // true: 1, false: -1
  private def sign(m: BigDecimal, b: BigDecimal, p: Point): BigDecimal = if (p.x(2) > m * p.x(1) + b) BigDecimal(1) else BigDecimal(-1)

  private def linearRegression(vectorSize: Int) {
    var X_pi = new Array[Array[BigDecimal]](vectorSize)
    val identity = new Array[Array[BigDecimal]](vectorSize)

    // (X transposed * X) and create identity matrix
    for (row <- 0 until vectorSize) {
      identity(row) = new Array[BigDecimal](vectorSize)
      X_pi(row) = new Array[BigDecimal](vectorSize)
      for (col <- 0 until vectorSize) {
        identity(row)(col) = BigDecimal(if (row == col) 1.0 else 0.0)
        var row_col = BigDecimal(0.0)
        for (i <- 0 until n) {
          row_col += x(i).x(row) * x(i).x(col)
        }
        X_pi(row)(col) = row_col
      }
    }

    //printMatrix(identity)
    //printMatrix(X_pi)

    // (X transposed * X)^-1 -- Gauss-Jordan inversion algo
    for (col <- 0 until vectorSize) {
      val divider = X_pi(col)(col)
      for (col1 <- 0 until vectorSize) {
        X_pi(col)(col1) = X_pi(col)(col1) / divider
        identity(col)(col1) = identity(col)(col1) / divider
      }
      for (row <- 0 until col) {
        val mult = X_pi(row)(col)
        for (col1 <- 0 until vectorSize) {
          X_pi(row)(col1) = X_pi(row)(col1) - X_pi(col)(col1) * mult
          identity(row)(col1) = identity(row)(col1) - identity(col)(col1) * mult
        }
      }
      for (row <- col + 1 until vectorSize) {
        val mult = X_pi(row)(col)
        for (col1 <- 0 until vectorSize) {
          X_pi(row)(col1) = X_pi(row)(col1) - X_pi(col)(col1) * mult
          identity(row)(col1) = identity(row)(col1) - identity(col)(col1) * mult
        }
      }
    }

    //printMatrix(identity)

    // multiply transpose of X
    X_pi = new Array[Array[BigDecimal]](vectorSize)
    for (row <- 0 until vectorSize) {
      X_pi(row) = new Array[BigDecimal](n)
      for (col <- 0 until n) {
        X_pi(row)(col) = BigDecimal(0.0)
        for (i <- 0 until vectorSize) {
          X_pi(row)(col) += identity(row)(i) * x(col).x(i)
        }
      }
    }

    //printMatrix(X_pi)

    // multiply y vector (f(x))
    w = new Array[BigDecimal](vectorSize)
    for (row <- 0 until vectorSize) {
      var row_col = BigDecimal(0.0)
      for (col <- 0 until n) {
        row_col += X_pi(row)(col) * f_x(col)
      }
      w(row) = row_col
    }
  }

  private def printMatrix(m: Array[Array[BigDecimal]]) {
    for (i <- 0 until m.length) {
      for (j <- 0 until m(i).length) 
        print(m(i)(j) + " ")
      println()
    }
    println()
  }

  private def printPoints {
    for (i <- 0 until x.length)
      println(x(i))
    println()
  }

  private def printWeights {
    for (i <- 0 until w.length)
      print(w(i) + " ")
    println()
  }


  /**
   * avg Ein ~ 0.038
   *
   * [c] 0.01
   */

  def experiment5(N: Int, runs: Int) {
    n = N
    var EinAvg = 0.0
    for (i <- 0 until runs) {
      w = new Array[BigDecimal](3)
      f(0) = new Point(3)
      f(1) = new Point(3)
      f_slope = slope(f)
      f_intercept = intercept(f)

      x = new Array[Point](n)
      f_x = new Array[BigDecimal](n)
      g_x = new Array[BigDecimal](n)

      for (j <- 0 until n) {
        x(j) = new Point(3)
        f_x(j) = sign(f_slope, f_intercept, x(j))
      }

      linearRegression(3)

      var Ein = 0
      for (j <- 0 until n) {
        g_x(j) = w(0) + w(1) * x(j).x(1) + w(2) * x(j).x(2)
        if ((f_x(j) > BigDecimal(0) && g_x(j) < BigDecimal(0)) || (f_x(j) < BigDecimal(0) && g_x(j) > BigDecimal(0))) {
          Ein += 1
        }
      }
      EinAvg += (Ein.toDouble / n)
    }
    println("Ein = " + (EinAvg / runs))     // ~0.038
  }

  /**
   * avg Eout ~ 0.047
   *
   * [c] 0.01
   */

  def experiment6(N: Int, runs: Int) {
    n = N
    var EoutAvg = 0.0
    for (i <- 0 until runs) {
      w = new Array[BigDecimal](3)
      f(0) = new Point(3)
      f(1) = new Point(3)
      f_slope = slope(f)
      f_intercept = intercept(f)

      x = new Array[Point](n)
      f_x = new Array[BigDecimal](n)
      g_x = new Array[BigDecimal](n)

      for (j <- 0 until n) {
        x(j) = new Point(3)
        f_x(j) = sign(f_slope, f_intercept, x(j))
      }

      linearRegression(3)

      var Eout = 0

      x = new Array[Point](1000)
      f_x = new Array[BigDecimal](1000)
      g_x = new Array[BigDecimal](1000)

      for (j <- 0 until 1000) {
        x(j) = new Point(3)
        f_x(j) = sign(f_slope, f_intercept, x(j))
        g_x(j) = w(0) + w(1) * x(j).x(1) + w(2) * x(j).x(2)
        if ((f_x(j) > BigDecimal(0) && g_x(j) < BigDecimal(0)) || (f_x(j) < BigDecimal(0) && g_x(j) > BigDecimal(0))) {
          Eout += 1
        }
      }
      EoutAvg += (Eout.toDouble / 1000)
    }
    println("Eout = " + (EoutAvg / runs))   // ~0.047
  }

/*
  def experiment7(N: Int, runs: Int) {
    def isClassified(i: Int): Boolean = f_x(i) == g_x(i)

    def allClassified: Boolean = {
      for (i <- 0 until n) {
        if (!isClassified(i))
          return false
      }
      return true
    }

    def calc_g_x(p: Point): Boolean =
      (w(0) * 1) + (w(1) * p.x) + (w(2) * p.y) > 0

    def classifyPoint(i: Int) {
      w(0) += (1 * sign2int(f_x(i)))
      w(1) += (x(i).x * sign2int(f_x(i)))
      w(2) += (x(i).y * sign2int(f_x(i)))
      for (j <- 0 until n)
        g_x(j) = calc_g_x(x(j))
    }

    n = N
    var it = 0
    for (i <- 0 until runs) {

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
    }
  }
*/

  /**
   * EinAvg_t = 0.48
   *
   * [d] 0.5
   */

  def experiment8 {
    n = 1000
    val runs = 1000
    var EinAvg = 0.0

    for (i <- 0 until runs) {
      f(0) = new Point(3)
      f(1) = new Point(3)
      x = new Array[Point](n)
      f_x = new Array[BigDecimal](n)

      for (j <- 0 until n) {
        x(j) = new Point(3)
        f_x(j) = x(j).x(1) * x(j).x(1) + x(j).x(2) * x(j).x(2) - BigDecimal(0.6)
        if (R.nextDouble < 0.1)
          f_x(j) = f_x(j) * BigDecimal(-1.0)
      }

      linearRegression(3)
      //printWeights

      var Ein = 0
      g_x = new Array[BigDecimal](n)
      for (j <- 0 until n) {
        g_x(j) = w(0) + w(1) * x(j).x(1) + w(2) * x(j).x(2)
        if ((f_x(j) > BigDecimal(0) && g_x(j) < BigDecimal(0)) || (f_x(j) < BigDecimal(0) && g_x(j) > BigDecimal(0))) {
          Ein += 1
        }
      }
      EinAvg += (Ein.toDouble / n)
    }
    println("EinAvg = " + (EinAvg / runs))
  }

  /**
   * w0 = -0.4972245217443033282995061773896796
   * w1 = 0.02049439431520522800187850162073198   ~ -0.05
   * w2 = 0.004638288554021677813538498929174463  ~ +0.08
   * w3 = 0.05197092066016114731010138485402840   ~ +0.13
   * w4 = 0.8772291404612958410396588816678301    ~ +1.5
   * w5 = 0.8433313740345393243373456642550035    ~ +1.5
   *
   * [a] g(x1 , x2) = sign(−1 − 0.05x1 + 0.08x2 + 0.13x1x2 + 1.5x1^2 + 1.5x2^2)
   */

  def experiment9 {
    n = 1000
    val runs = 1
    
    for (i <- 0 until runs) {
      f(0) = new Point(3)
      f(1) = new Point(3)
      x = new Array[Point](n)
      f_x = new Array[BigDecimal](n)

      for (j <- 0 until n) {
        x(j) = new Point(3)
        f_x(j) = x(j).x(1) * x(j).x(1) + x(j).x(2) * x(j).x(2) - BigDecimal(0.6)
        if (R.nextDouble < 0.1)
          f_x(j) = f_x(j) * BigDecimal(-1.0)

        x(j) transform 6
        x(j).x(3) = x(j).x(1) * x(j).x(2)
        x(j).x(4) = x(j).x(1) * x(j).x(1)
        x(j).x(5) = x(j).x(2) * x(j).x(2)
      }

      //printPoints
      linearRegression(x(0).x.length)
      printWeights
    }
  }

  /**
   * EoutAvg = 0.11101899999999969
   *
   * [b] 0.1
   */

  def experiment10 {
    n = 1000
    val runs = 1000
    
    f(0) = new Point(3)
    f(1) = new Point(3)
    x = new Array[Point](n)
    f_x = new Array[BigDecimal](n)

    for (j <- 0 until n) {
      x(j) = new Point(3)
      f_x(j) = x(j).x(1) * x(j).x(1) + x(j).x(2) * x(j).x(2) - BigDecimal(0.6)
      if (R.nextDouble < 0.1)
        f_x(j) = f_x(j) * BigDecimal(-1.0)

      x(j) transform 6
      x(j).x(3) = x(j).x(1) * x(j).x(2)
      x(j).x(4) = x(j).x(1) * x(j).x(1)
      x(j).x(5) = x(j).x(2) * x(j).x(2)
    }

    //printPoints
    linearRegression(x(0).x.length)
    //printWeights

    var EoutAvg = 0.0
    for (i <- 0 until runs) {
      //g_x = new Array[BigDecimal](n)
      var Eout = 0
      for (j <- 0 until 1000) {
        val p = new Point(6)
        p.x(3) = p.x(1) * p.x(2)
        p.x(4) = p.x(1) * p.x(1)
        p.x(5) = p.x(2) * p.x(2)

        var f_of_p: BigDecimal = ( p.x(1) * p.x(1) ) + ( p.x(2) * p.x(2) ) - BigDecimal(0.6)
        if (R.nextDouble < 0.1)
          f_of_p = f_of_p * BigDecimal(-1.0)
        val g_of_p: BigDecimal = p.x(0) * w(0) + p.x(1) * w(1) + p.x(2) * w(2) + p.x(3) * w(3) + p.x(4) * w(4) + p.x(5) * w(5)
        if (g_of_p > 0.0 && f_of_p < 0.0 || g_of_p < 0.0 && f_of_p > 0.0)
          Eout += 1
      }
      EoutAvg += (Eout.toDouble / 1000)
    }
    println("EoutAvg = " + (EoutAvg / runs))
  }

  def main(args: Array[String]) {
    //this.experiment5(100, 1000)
    //this.experiment6(100, 1000)
    //this.experiment8
    this.experiment9
    //this.experiment10
  }
}

