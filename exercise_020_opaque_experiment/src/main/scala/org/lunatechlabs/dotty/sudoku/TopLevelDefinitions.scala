package org.lunatechlabs.dotty.sudoku

import ReductionSets.{ReductionSet, Sudoku}

private val N = 9
val CELLPossibleValues: Vector[Int] = (1 to N).toVector
val cellIndexesVector: Vector[Int] = Vector.range(0, N)
val initialCell: Set[Int] = Set.range(1, 10)

// type CellContent = Set[Int]
// type ReductionSet = Vector[CellContent]
// type Sudoku = Vector[ReductionSet]

type CellUpdates = Vector[(Int, Set[Int])]
val cellUpdatesEmpty = Vector.empty[(Int, Set[Int])]

import SudokuDetailProcessor.RowUpdate

def (update: Vector[SudokuDetailProcessor.RowUpdate]).toSudokuField: SudokuField = {
  import scala.language.implicitConversions
  val rows =
        update
          .map { case SudokuDetailProcessor.RowUpdate(id, cellUpdates) => (id, cellUpdates)}
          .to(Map).withDefaultValue(cellUpdatesEmpty)
  val sudoku = for {
    (row, cellUpdates) <- Vector.range(0, 9).map(row => (row, rows(row)))
    x = cellUpdates.to(Map).withDefaultValue(Set(0))
    y = Vector.range(0, 9).map(n => x(n))
    } yield y
  SudokuField(Sudoku(sudoku))
}

// Collective Extensions:
// define extension methods that share the same left-hand parameter type under a single extension instance.
extension sudokuFieldOps on (sudokuField: SudokuField) {

  def transpose: SudokuField = SudokuField(sudokuField.sudoku.transpose)

  def rotateCW: SudokuField = SudokuField(sudokuField.sudoku.rotateCW)

  def rotateCCW: SudokuField = SudokuField(sudokuField.sudoku.rotateCCW)

  def flipVertically: SudokuField = SudokuField(sudokuField.sudoku.flipVertically)

  def flipHorizontally: SudokuField = sudokuField.rotateCW.flipVertically.rotateCCW

  def rowSwap(row1: Int, row2: Int): SudokuField = {
    SudokuField(sudokuField.sudoku.rowSwap(row1, row2))
  }

  def columnSwap(col1: Int, col2: Int): SudokuField = {
    sudokuField.rotateCW.rowSwap(col1, col2).rotateCCW
  }

  def randomSwapAround: SudokuField = {
    SudokuField(sudokuField.sudoku.randomSwapAround)
  }

  def toRowUpdates: Vector[SudokuDetailProcessor.RowUpdate] = {
    sudokuField
      .sudoku
      .toRowUpdates
  }
}
