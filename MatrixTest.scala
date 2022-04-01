import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._

import scala.util.Success

class MatrixTest extends AnyWordSpec {
  "Matrix" when {
    "toList called" should {
      "return this matrix in List format" in {
        new Matrix(List[List[Double]](List(1, 1, 1), List(2,2,2), List(3,3,3), List(4,4,4))).toList shouldBe List[List[Double]](List(1, 1, 1), List(2,2,2), List(3,3,3), List(4,4,4))
      }
    }
  }

  "Matrix" when {
    "transpose called" should {
      "return new transpose matrix" in {
        (new Matrix(List[List[Double]](List(1, 1, 1), List(2,2,2), List(3,3,3), List(4,4,4))).transpose).toList shouldBe List[List[Double]](List(1, 1, 1), List(2,2,2), List(3,3,3), List(4,4,4)).transpose
      }
    }
  }

  "Matrix" when {
    "add called" should {
      "return sum of 2 matrices" in {
        (new Matrix(List[List[Double]](List(1,1), List(2,2))).add(new Matrix(List[List[Double]](List(1,1), List(2,2))))).toList shouldBe List[List[Double]](List(2,2), List(4,4))
      }
    }
  }

  "Matrix" when {
    "* operator" should {
      "return new matrix" in {
        (new Matrix(List[List[Double]](List(1,1), List(2,2))) * new Matrix(List[List[Double]](List(1,1), List(2,2)))).toList shouldBe List[List[Double]](List(3,3), List(6,6))
      }
    }
  }

  "Matrix" when {
    "toDisplay called" should {
      "return matrix in string format to display this in terminal" in {
        new Matrix(List[List[Double]](List(1,1), List(2,2))).toDisplay shouldBe List[List[Double]](List(1,1), List(2,2)).map(_.mkString(" ")).mkString("\n")
      }
    }
  }
}
