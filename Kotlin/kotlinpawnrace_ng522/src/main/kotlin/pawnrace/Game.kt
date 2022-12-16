package pawnrace

import kotlin.math.abs

class Game(val whiteGap: Int, val blackGap: Int) {
    var board = mutableListOf<MutableList<Int>>()
    var player = -1
    var lastMove = Pair(-1, -1)
    init {
        for ( i in 0..7) {
            board.add(mutableListOf())
            for ( j in 0..7) {
                board.last().add(0)
            }
        }

        for ( i in 0..7) {
            if ( i != whiteGap)
                board [1][i] = -1
            if ( i != blackGap)
                board [6][i] = 1
        }
    }

    override fun toString(): String {
        var s = ""
        for ( i in 0..7) {
            var r = (i+1).toString() + "  "
            for ( j in 0..7) {
                if ( board [i][j] == 0 )r += ". "
                else if (board [i][j] == -1)r += "W "
                else if (board [i][j] == 1)r += "B "
                else r += "# "
            }
            s = r + " " + (i+1).toString() + "\n" + s
        }
        return "   a b c d e f g h\n" + s + "   a b c d e f g h"
    }

    fun formatMove(r: Int, c: Int, take: Int): String {
        if ( take >= 0)return (take + 'a'.code).toChar().toString() + "x" + (c + 'a'.code).toChar() + (r+1).toString()
        else return (c + 'a'.code).toChar() + (r+1).toString()
    }

    fun getMoves(): Map<String, Pair<Int, Int>> {
        var moves = mutableMapOf<String, Pair<Int, Int>>()
        for ( i in 0..7) {
            for ( j in 0..7) {
                var d = board [i][j]
                if ( d == 0 || d == -2 || d != player)continue
                if ( board [i-d][j] == 0 ) {
                    moves [formatMove(i-d, j, -1)] = i to j
                    if ( i-2*d in 0..7 && board [i-2*d][j] == 0 ) {
                        if ( d == 1 && i == 6)moves [formatMove(i-2*d, j, -1)] = i to j
                        else if ( d == -1 && i == 1)moves [formatMove(i-2*d, j, -1)] = i to j
                    }
                }
                if ( j > 0 )
                    if ( board [i-d][j-1] == -d || board [i-d][j-1] == 2) moves [formatMove(i-d, j-1, j)] = i to j
                if ( j < 7)
                    if ( board [i-d][j+1] == -d || board [i-d][j+1] == 2) moves [formatMove(i-d, j+1, j)] = i to j

            }
        }
        return moves
    }

    fun isValidMove(move: String): Boolean {
        return getMoves().containsKey(move)
    }

    fun applyMove(m: String): Boolean {
        val p = getMoves() [m]!!
        board [p.first][p.second] = 0

        var off = 0
        if ( m [1] == 'x' )off = 2
        val r = m [1+off].code - '1'.code
        val c = m [0+off].code - 'a'.code

        if ( board [r][c] == 2 ) {
            board [r-1][c] = 0
            board [r+1][c] = 0
        }
        board [r][c] = player

        if ( abs(r - p.first) == 2)return true
        return false
    }

    fun removeGhost() {
        if ( lastMove.first >= 0 && board [lastMove.first][lastMove.second] == 2) {
            board [lastMove.first][lastMove.second] = 0
            lastMove = Pair(-1, -1)
        }
    }

    fun addGhost(m: String, prev: Pair<Int, Int>) {
        val r = m [1].code - '1'.code
        val nr = prev.first
        val c = prev.second
        val mr = (r + nr)/2
        board [mr][c] = 2
        lastMove = Pair(mr, c)
    }

    fun checkWin(): Int? {
        var win = 0
        for ( i in 0..7 ) {
            if ( board [0][i] != 0 )win = board [0][i]
            if ( board [7][i] != 0 )win = board [7][i]
        }
        if ( win == 0 ) {
            if ( getMoves().isEmpty() )return 0
            else return null
        }
        else return win
    }

    fun makeMove(m: String): Boolean {
        if ( !isValidMove(m) )return false
        val prev = getMoves() [m]!!
        val passant = applyMove(m)
        removeGhost()
        if ( passant ) addGhost(m, prev)
        player *= -1
        return true
    }

    fun copy(): Game {
        val g = Game(whiteGap, blackGap)
        g.board = board.map{it.toList().toMutableList()}.toMutableList()
        g.player = player
        g.lastMove = lastMove
        return g
    }

    fun getPawns(): Pair<List<Pair<Int, Int>>, List<Pair<Int, Int>>> {
        var a = mutableListOf<Pair<Int, Int>>()
        var b = mutableListOf<Pair<Int, Int>>()
        for ( i in 0..7 ) {
            for ( j in 0..7 ) {
                if ( board [i][j] == 1)a.add(i to j)
                else if (board [i][j] == -1)b.add(i to j)
            }
        }
        return (a to b)
    }

    fun aiMove(): String {
        val ai = AI(this, player)

        val move = ai.getMove()

        return move.first
    }
}

fun main() {
    var b = Game(2, 3)

    while ( b.checkWin() == null) {
        println(b)
        var input = readln()
        while ( !b.makeMove(input))
            input = readln()
    }

    println(b)
}