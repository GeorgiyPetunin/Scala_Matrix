object ScalaLab1{
  def main(args: Array[String]): Unit = {

    val vectorA = FillingVector(3)
    VectorDisplay(vectorA, "VectorA")
    val vectorB = FillingVector(3)
    VectorDisplay(vectorB, "VectorB")

    VectorDisplay(VectorMultiplication(vectorA, vectorB), "Vector multiplication")
    println("Scalar vector multiplication\n" + ScalarMultiplication(vectorA, vectorB))
    println("Reverse vector\n" + ReverseVector(vectorA))

  }

  def VectorDisplay(a : List[Double], b : String) : Unit = {
    println(b)
    println(a.mkString("\n"))
  }

  def FillingVector(size : Int) : List[Double] = {
    List.fill(size){scala.math.sin(util.Random.nextInt())}
  }

  def VectorMultiplication(a : List[Double], b : List[Double]) : List[Double] = {
    a(1)*b(2) - a(2)*b(1) :: a(2)*b.head - a.head*b(2) :: a.head*b(1) - a(1)*b.head :: Nil
  }

  def ScalarMultiplication(a : List[Double], b : List[Double]) : Double = {
    a.zip(b).map{
      case (a, b) => a * b
    }.sum
  }

  def ReverseVector(a : List[Double]) : List[Double] = {
    a.reverse
  }
}