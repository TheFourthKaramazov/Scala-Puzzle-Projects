// Sudoku board as a multidimensional array
val board: Array[Array[Int]] = Array.ofDim[Int](9, 9)

board(0) = Array(9, 0, 0, 1, 8, 0, 3, 0, 0)
board(1) = Array(0, 0, 0, 0, 0, 0, 0, 4, 0)
board(2) = Array(0, 0, 5, 0, 0, 7, 0, 0, 2)
board(3) = Array(4, 0, 0, 0, 1, 8, 6, 0, 0)
board(4) = Array(8, 0, 0, 6, 0, 4, 0, 0, 5)
board(5) = Array(0, 0, 7, 5, 3, 0, 0, 0, 8)
board(6) = Array(7, 0, 0, 8, 0, 0, 9, 0, 0)
board(7) = Array(0, 2, 0, 0, 0, 0, 0, 0, 0)
board(8) = Array(0, 0, 9, 0, 7, 1, 0, 0, 4)

// Utility function to display the Sudoku board
def boardDisplay(board: Array[Array[Int]]): Unit = {
board.foreach { row =>
    row.foreach { cell =>
    print(cell + " ")
    }
    println()
}
}

// Function to find an empty cell (cell with value 0)
def findEmptyCell(board: Array[Array[Int]]): Option[(Int, Int)] = {
val cells = for {
    row <- 0 until 9
    col <- 0 until 9
    if board(row)(col) == 0
} yield (row, col)

cells.headOption
}

// Function to validate if a value can be placed in a cell
def isValid(board: Array[Array[Int]], row: Int, col: Int, num: Int): Boolean = {
val rowValid = !board(row).contains(num)
val colValid = !board.map(_(col)).contains(num)
val squareValid = {
    val startRow = 3 * (row / 3)
    val startCol = 3 * (col / 3)
    !board.slice(startRow, startRow + 3).flatMap(_.slice(startCol, startCol + 3)).contains(num)
}
rowValid && colValid && squareValid
}
// Recursive Sudoku solver function that returns all solutions
def solveSudoku(board: Array[Array[Int]]): List[Array[Array[Int]]] = {
findEmptyCell(board) match {
    case Some((row, col)) =>
    (1 to 9).flatMap { num =>
        if (isValid(board, row, col, num)) {
        val newBoard = board.map(_.clone())
        newBoard(row)(col) = num
        solveSudoku(newBoard)
        } else List()
    }.toList
    case None => List(board.clone())
}
}

// Solve and display all Sudoku solutions
val solutions = solveSudoku(board)
if (solutions.nonEmpty) {
println(s"Found ${solutions.length} solution(s):")
solutions.foreach { solution =>
    boardDisplay(solution)
    println()
}
} else {
println("No solution found for the Sudoku.")
}
