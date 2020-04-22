package akkapi.cluster

package object sudoku {

  private val N = 9
  val CELLPossibleValues: Vector[Int] = (1 to N).toVector
  val cellIndexesVector: Vector[Int] = (0 until N).toVector // TODO - Refactor using Vector.range
  val initialCell: Set[Int] = Set(1 to N: _*)

  type CellContent = Set[Int]
  type ReductionSet = Vector[CellContent]
  type Sudoku = Vector[ReductionSet]

  type CellUpdates = Vector[(Int, Set[Int])]
  val cellUpdatesEmpty = Vector.empty[(Int, Set[Int])]
}
