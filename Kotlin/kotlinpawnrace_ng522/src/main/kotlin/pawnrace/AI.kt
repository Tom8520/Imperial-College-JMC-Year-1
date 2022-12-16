package pawnrace

import kotlin.concurrent.thread
class AI(val g: Game, val player: Int, val depth: Int = 5) {

    fun getMove(): Pair<String, Int> {
        if ( depth == 0 ) {
            return ("" to getScore())
        }
        if ( g.checkWin() != null ) {
            if ( g.checkWin() == player ) return ("" to 999999)
            else return ("" to -999999)
        }
        var maxScore = -9999999
        if ( g.player != player ) maxScore = 99999999
        var chosenMove = ""

        for (move in g.getMoves().keys) {

                val newGame = g.copy()
                newGame.makeMove(move)
                val score = AI(newGame, player, depth - 1).getMove()
                if (player == g.player && score.second > maxScore) {
                    maxScore = score.second
                    chosenMove = move
                    if ( maxScore == 999999 )break
                } else if (player != g.player && score.second < maxScore) {
                    maxScore = score.second
                    chosenMove = move
                    if ( maxScore == -999999)break
                }

        }

        return (chosenMove to maxScore)
    }

    fun getScore(): Int{
        if ( g.checkWin() == null ) {
            val pawns = g.getPawns()
            var score = 0
            var avgBlack = pawns.first.fold(0) {x, y -> x + 7 - y.first}
            var avgWhite = pawns.first.fold(0) {x, y -> x + y.first}

            if ( player == -1 )score += avgWhite
            else score += avgBlack

            score += 5 * player * (pawns.first.size - pawns.second.size)

            return 10 * score + (-9..9).random()
        } else {
            if ( g.checkWin() == player) return 999999
            else return -999999
        }
    }
}