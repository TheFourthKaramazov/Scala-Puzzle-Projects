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

// Define function to display Sudoku board
def boardDisplay(board: Array[Array[Int]]): Unit = {
for (i <- 0 until board.length) {
    if (i % 3 == 0 && i != 0) println("_____________________")

    for (j <- 0 until board(0).length) {
    if (j % 3 == 0 && j != 0) print("|" + " ")
    if (j == 8) println(board(i)(j))
    else print(board(i)(j) + " ")
    }
}
}

// Search for empty entries on Sudoku board
def searchEmpty(board: Array[Array[Int]]): Option[(Int, Int)] = {
for (i <- 0 until board.length) {
    for (j <- 0 until board(0).length) {
    if (board(i)(j) == 0) return Some((i, j))
    }
}
None
}

// Validate board - ensure that board is a valid Sudoku board
def validateSudoku(board: Array[Array[Int]], row: Int, col: Int, value: Int): Boolean = {
// Check the row, column, and 3x3 square for the validity of the value
val rowValid = !board(row).contains(value)
val colValid = !board.map(_(col)).contains(value)
val squareValid = {
    val startRow = 3 * (row / 3)
    val startCol = 3 * (col / 3)
    !board.slice(startRow, startRow + 3).flatMap(_.slice(startCol, startCol + 3)).contains(value)
}
rowValid && colValid && squareValid
}

// Backtracking algorithm to solve Sudoku board
def solveBoard(board: Array[Array[Int]]): Boolean = {
val search = searchEmpty(board)
if (search.isEmpty) return true
val (row, col) = search.get

for (i <- 1 to 9) {
    if (validateSudoku(board, row, col, i)) {
    board(row)(col) = i
    if (solveBoard(board)) return true
    board(row)(col) = 0
    }
}
false
}

// Solve and display the Sudoku board
if (solveBoard(board)) {
println("Solved Sudoku:")
boardDisplay(board)
} else {
println("The sodoku that you have listed in not solvable. Please verify that your board is valid.")
}