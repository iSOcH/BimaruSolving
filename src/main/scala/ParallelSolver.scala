import java.nio.ByteBuffer
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

trait ParallelSolver extends SolverHelper {
  lazy val solutions: Stream[BimaruBoard] = {
    val counter = new AtomicLong()
    val tried = new ConcurrentHashMap[ByteBuffer, Unit]()

    possibleSteps.toStream.flatMap { changes =>
      solveRec(concluded.updated(changes), 0, tried, counter)
    }
  }

  override def updated(p: Pos, c: Cell): BimaruBoard with ParallelSolver = updated(Seq(p -> c))

  override def updated(changes: Seq[(Pos,Cell)]): BimaruBoard with ParallelSolver = {
    val newState = super.updated(changes).state
    new BimaruBoard(ships, occInRows, occInCols, newState) with ParallelSolver
  }
}

object ParallelSolverImplicit {
  implicit class convertToParallel(board: BimaruBoard) {
    def withParallelSolver: BimaruBoard with ParallelSolver =
      new BimaruBoard(board.ships, board.occInRows, board.occInCols, board.state) with ParallelSolver
  }
}

private object solveRec {
  val PARALLEL_RECURSION_DEPTH_LIMIT = 3

  def apply(board:SolverHelper, depth: Int, triedStates: ConcurrentHashMap[ByteBuffer, Unit], counter: AtomicLong): Seq[BimaruBoard] = {
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
        val boards = parOrStreamSteps.map(c => c.length -> board.concluded.updated(c))
        val neededLength = boards.find(_._2.rulesSatisfied).map(_._1)
        for {
          (length, board) <- boards
          if neededLength.contains(length) && board.rulesSatisfied && board.concluded.rulesSatisfied
          solution <- solveRec(board.concluded, depth+1, triedStates, counter)
        } yield {
          solution
        }
      }.seq
    }
  }
}
