object ScalaLab1{
  def main(args: Array[String]): Unit = {

    println("vector A")
    val vectorA = FillingSin(3)
    VectorDisplay(vectorA)
    println("Vector B")
    val vectorB = FillingCos(3)
    VectorDisplay(vectorB)

    VectorDisplay(VectorMultiplication(vectorA, vectorB), "Vector multiplication")
    println("Scalar vector multiplication")
    println(ScalarMultiplication(vectorA, vectorB))
  }

  def VectorDisplay(a : Array[Double]) : Unit = {
    val resultVector = a.mkString("\n")
    println(resultVector)
  }

  def VectorDisplay(a : Array[Double], b : String) : Unit = {
    println(b)
    VectorDisplay(a)
  }

  def FillingSin(size : Int) : Array[Double] = {
    val a = new Array[Double](size)
    for(i <- 0 until size){
      a(i) = Math.sin(i)
    }
    a
  }

  def FillingCos(size : Int) : Array[Double] = {
    val b = new Array[Double](size)
    for(i <- 0 until size){
      b(i) = Math.cos(i)
    }
    b
  }

  def VectorMultiplication(a : Array[Double], b : Array[Double]) : Array[Double] = {
    if(a.length != 3 && b.length != 3)
      throw new IllegalArgumentException

    val result = new Array[Double](3)
      result(0) = a(1)*b(2) - a(2)*b(1)
      result(1) = a(2)*b(0) - a(0)*b(2)
      result(2) = a(0)*b(1) - a(1)*b(0)

    result
  }

  def ScalarMultiplication(a : Array[Double], b : Array[Double]) : Double = {
    if(a.length != b.length)
      throw new IllegalArgumentException

    var result : Double = 0
    for(i <- a.indices)
      result += a(i) * b(i)

    result
  }
}
