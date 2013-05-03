import scala.util.Random
import scala.math._

object Hw4 {
  val r = new Random
  def point = r.nextDouble * (if (r.nextBoolean) 1 else -1)

  def main(argv: Array[String]) {
    val runs = 10000
    val x: Array[(Double, Double, Double, Double)] = new Array[(Double, Double, Double, Double)](runs)
    val slopes: Array[Double] = new Array[Double](runs)
    var avg_slope: Double = 0.0
    var mse = 0.0
    var a = 0.0
    
    for (i <- 0 until runs) {
      val x1_coord = point
      val x2_coord = point
      val y1_coord = sin(Pi * x1_coord)
      val y2_coord = sin(Pi * x2_coord)
      x(i) = (x1_coord, y1_coord, x2_coord, y2_coord)
      slopes(i) = ((x1_coord * y1_coord) + (x2_coord * y2_coord)) / ((x1_coord * x1_coord) + (x2_coord * x2_coord))
      avg_slope += slopes(i)

      //println(x(i))
      //println(slopes(i))
      //println(slopes(i) * x1_coord)

    }
    a = avg_slope / runs
    println(a)

    for (i <- 0 until runs) {
      val x1_coord = point
      val y1_coord = sin(Pi * x1_coord)
      
      val g_x = x1_coord * a
      mse += (g_x - y1_coord) * (g_x - y1_coord)
    }
    println("bias = " + mse / runs)

    var variance = 0.0
    for (i <- 0 until runs) {
      val x1_coord = point
      val x2_coord = point
      val y1_coord = sin(Pi * x1_coord)
      val y2_coord = sin(Pi * x2_coord)
      x(i) = (x1_coord, y1_coord, x2_coord, y2_coord)
      slopes(i) = ((x1_coord * y1_coord) + (x2_coord * y2_coord)) / ((x1_coord * x1_coord) + (x2_coord * x2_coord))

      val g_x = x1_coord * a
      val g_x_D = x1_coord * slopes(i)
      variance += (g_x_D - g_x) * (g_x_D - g_x)
    }
    println("variance = " + variance / runs)
  }
 }

