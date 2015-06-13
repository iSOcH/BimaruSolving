import java.nio.ByteBuffer
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

import SolverHelper.makeHelped

object ParallelSolver {
  implicit class makeParallel(board: BimaruBoard) {
    lazy val solutions: Stream[BimaruBoard] = {
      val counter = new AtomicLong()
      val tried = new ConcurrentHashMap[ByteBuffer, Unit]()

      board.possibleSteps.toStream.flatMap { changes =>
        solveRec(board.updatedAndConclude(changes), 0, tried, counter)
      }
    }
  }

  val PARALLEL_RECURSION_DEPTH_LIMIT = 3

  private def solveRec(board: BimaruBoard,
                       depth: Int,
                       triedStates: ConcurrentHashMap[ByteBuffer, Unit],
                       counter: AtomicLong): Seq[BimaruBoard] = {
    if (counter.incrementAndGet() % 256 == 0) {
      println()
      println(triedStates.size())
      println(board + "\n")
    }

    val alreadyChecked = () equals triedStates.putIfAbsent(board.uniqueID, ())
    if (alreadyChecked) {
      Seq.empty
    } else {
      if (board.isSolved) {
        Seq(board)
      } else {
        val parOrStreamSteps = if (depth < PARALLEL_RECURSION_DEPTH_LIMIT) {
          board.possibleSteps.par
        } else board.possibleSteps.toStream

        /* after we found a change using a 4-field-ship that was valid,
        we should not try to add a 3-field-ship instead, because we will
        need to add the 4-field-ship anyways
        this optimization significantly improves performance (about factor 3) */
        val boards = parOrStreamSteps.map(c => c.length -> board.updatedAndConclude(c))

        // steps are sorted by length descending, thus neededLength is the longest
        // length which yields a valid board
        val neededLength = boards.find(_._2.rulesSatisfied).map(_._1)
        for {
          (length, changedBoard) <- boards
          if neededLength.contains(length) && changedBoard.rulesSatisfied
          solution <- solveRec(changedBoard, depth + 1, triedStates, counter)
        } yield {
          solution
        }
      }.seq
    }
  }
}
