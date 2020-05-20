package org.lunatechlabs.dotty.sudoku

object ReductionSets {
  type CellContent = Set[Int]
  opaque type ReductionSet = Vector[CellContent]
  opaque type Sudoku = Vector[ReductionSet]

  object Sudoku {
    def apply(s: Vector[Vector[CellContent]]): Sudoku = s

    import SudokuProgressTracker.SudokuDetailState
    def toSudoku(state: Vector[SudokuDetailState]): Sudoku = {
      Sudoku(state.sortBy { case SudokuDetailState(idx, _) => idx }.map { case SudokuDetailState(_, state) => state})
    }
  }

  object ReductionSet {
    def initial: ReductionSet = cellIndexesVector.map(_ => initialCell)

    def apply(cells: Vector[CellContent]) = cells

    def mergeState(state: ReductionSet, cellUpdates: CellUpdates): ReductionSet = {
      (cellUpdates foldLeft state) {
      case (stateTally, (index, updatedCellContent)) =>
        stateTally.updated(index, stateTally(index) & updatedCellContent)
      }
    }
    
    def stateChanges(state: ReductionSet, updatedState: ReductionSet): CellUpdates = {
      ((state zip updatedState).zipWithIndex foldRight cellUpdatesEmpty) {
        case (((previousCellContent, updatedCellContent), index), cellUpdates)
          if updatedCellContent != previousCellContent =>
          (index, updatedCellContent) +: cellUpdates

        case (_, cellUpdates) => cellUpdates
      }
    }

    def printRow( row: ReductionSet): String = {
      def printSubRow( subRowNo: Int): String = {
        val printItems = List(1,2,3) map( x => x + subRowNo * 3)
        (for  (elem <- row) 
          yield {
            (printItems map (item => if ((elem & printItems.toSet).contains(item)) item.toString else " ")).mkString("")
          }).mkString("| ", " | ", " |")
      }
      (for  (subRow <- 0 until 3)  yield printSubRow(subRow)).mkString("\n")
    }

    def printRowShort( row: ReductionSet): String = {
      (for
        (elem <- row)
      yield {
        if (elem.size == 1) elem.head.toString else " "
      }).mkString("|","|","|")

    }

    private def sudokuCellRepresentation(content: CellContent): String = {
      content.toList match {
        case Nil => "x"
        case singleValue +: Nil => singleValue.toString
        case _ => " "
      }
    }

    def sudokuRowPrinter(threeRows: Vector[ReductionSet]): String = {
      val rowSubBlocks = for {
        row <- threeRows
        rowSubBlock <- row.map(el => sudokuCellRepresentation(el)).sliding(3,3)
        rPres = rowSubBlock.mkString

      } yield rPres
      rowSubBlocks.sliding(3,3).map(_.mkString("", "|", "")).mkString("|", "|\n|", "|\n")
    }

    def sudokuPrinter(result: SudokuSolver.SudokuSolution): String = {
      result.sudoku.pr
    }
  }

  extension sudokuExtensions on (sudoku: Sudoku) {
    def pr: String = sudoku.sliding(3,3).map(ReductionSet.sudokuRowPrinter).mkString("\n+---+---+---+\n", "+---+---+---+\n", "+---+---+---+")
    def transpose: Sudoku = sudoku.transpose
    def rotateCW: Sudoku = sudoku.reverse.transpose
    def rotateCCW: Sudoku = sudoku.transpose.reverse
    def flipVertically: Sudoku = sudoku.reverse
    def rowSwap(row1: Int, row2: Int): Sudoku =
      sudoku.zipWithIndex.map {
            case (_, `row1`) => sudoku(row2)
            case (_, `row2`) => sudoku(row1)
            case (row, _) => row
          }
    def randomSwapAround: Sudoku = {
      import scala.language.implicitConversions
      val possibleCellValues = Vector(1,2,3,4,5,6,7,8,9)
      // Generate a random swapping of cell values. A value 0 is used as a marker for a cell
      // with an unknown value (i.e. it can still hold all values 0 through 9). As such
      // a cell with value 0 should remain 0 which is why we add an entry to the generated
      // Map to that effect
      val shuffledValuesMap =
        possibleCellValues.zip(scala.util.Random.shuffle(possibleCellValues)).to(Map) + (0 -> 0)
      sudoku.map { row =>
        row.map(cell => Set(shuffledValuesMap(cell.head)))
      }
    }

    def toRowUpdates: Vector[SudokuDetailProcessor.RowUpdate] = {
      sudoku
        .map(_.zipWithIndex)
        .map(row => row.filterNot(_._1 == Set(0)))
        .zipWithIndex.filter(_._1.nonEmpty)
        .map { (c, i) =>
          SudokuDetailProcessor.RowUpdate(i, c.map(_.swap))
        }
  }
  }

  extension reductionRules on (reductionSet: ReductionSet) {

    def isFullyReduced: Boolean = {
      val allValuesInState = reductionSet.flatten
      allValuesInState == allValuesInState.distinct
    }

    def applyReductionRuleOne: ReductionSet = {
      val inputCellsGrouped = reductionSet filter {_.size <= 7} groupBy identity
      val completeInputCellGroups = inputCellsGrouped filter { (set, setOccurrences) =>
        set.size == setOccurrences.length
      }
      val completeAndIsolatedValueSets = completeInputCellGroups.keys.toList
      (completeAndIsolatedValueSets foldLeft reductionSet) { (cells, caivSet) =>
        cells map {
          cell => if (cell != caivSet) cell &~ caivSet else cell
        }
      }
    }

    def applyReductionRuleTwo: ReductionSet = {
      val valueOccurrences = CELLPossibleValues map { value =>
        (cellIndexesVector zip reductionSet foldLeft Vector.empty[Int]) {
          case (acc, (index, cell)) =>
            if (cell contains value) index +: acc else acc
        }
      }

      val cellIndexesToValues =
        (CELLPossibleValues zip valueOccurrences)
          .groupBy ((value, occurrence) => occurrence)
          .filter  ((loc, occ) => loc.length == occ.length && loc.length <= 6 )

      val cellIndexListToReducedValue = cellIndexesToValues map { (index, seq) =>
        (index, (seq map ((value, _) => value )).toSet)
      }

      val cellIndexToReducedValue = cellIndexListToReducedValue flatMap { (cellIndexList, reducedValue) => 
        cellIndexList map { cellIndex => cellIndex -> reducedValue }
      }

      (reductionSet.zipWithIndex foldRight Vector.empty[CellContent]) {
        case ((cellValue, cellIndex), acc) =>
          cellIndexToReducedValue.getOrElse(cellIndex, cellValue) +: acc
      }
    }
  }
}
