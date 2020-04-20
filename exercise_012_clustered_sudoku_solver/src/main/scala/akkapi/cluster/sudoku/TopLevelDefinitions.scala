package akkapi.cluster.sudoku

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
      SudokuField(sudoku)
}
