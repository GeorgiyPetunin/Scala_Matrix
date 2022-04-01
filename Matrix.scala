import util.control.Breaks._

class Matrix(matrix : List[List[Double]]) {

  val Rows: Int = matrix.length
  val Column: Int = matrix.head.length
  private var _matrix: List[List[Double]] = matrix

  def setMatrix(matrix : Matrix) : Unit = {
    _matrix = matrix.toList
  }

  def toMatrix(list: List[List[Double]]): Matrix = {
    new Matrix(list)
  }

  def toList: List[List[Double]] = {
    _matrix
  }

  def toDisplay : String = {
    _matrix.map(_.mkString(" ")).mkString("\n")
  }

  def transpose : Matrix = {
    toMatrix(_matrix.transpose)
  }

  def add(matrix: Matrix) : Matrix = {
    toMatrix(this.matrix) + matrix
  }

  def sub(matrix: Matrix) : Matrix = {
    toMatrix(this.matrix) - matrix
  }

  def multiply(matrix: Matrix) : Matrix = {
    toMatrix(this.matrix) * matrix
  }

  def +(matrix1: Matrix) : Matrix = {
    toMatrix(_matrix.zip(matrix1.toList).map {
      case (x1, x2) => x1.zip(x2).map {
        case (y1, y2) => y1 + y2
      }
    }
    )
  }

  def -(matrix1: Matrix): Matrix = {
    toMatrix(_matrix.zip(matrix1.toList).map {
      case (x1, x2) => x1.zip(x2).map {
        case (y1, y2) => y1 - y2
      }
    }
    )
  }

  def *(matrix1: Matrix) : Matrix = {
    toMatrix(
      _matrix.map(i =>
        List.tabulate(matrix1.Column)(
          matrix1.toList.transpose.map(
            _.zip(i).map {
              case (x1, x2) => x1 * x2
            }.sum
          )
        )
      )
    )
  }

  def *(value : Int) : Matrix = {
    toMatrix(_matrix.map(_.map(_ * value)))
  }

  def /(value : Int) : Matrix = {
    toMatrix(_matrix.map(_.map(_ / value)))
  }
//  def TrueMultiply(matrix : Matrix) : Unit = {
//    breakable{
//      if(Column == matrix.Rows)
//        (_matrix, matrix).swap
//      else if(Column != matrix.Rows || Rows != matrix.Column)
//        break
//    }
//  }
}

