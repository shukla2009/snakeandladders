
/**
 * Created by Rahul Shukla <rahul.shukla@synerzip.com> on 21/9/15.
 */
case class Snake(start: Int, end: Int) {
  require(end > 0, "End of snake should be greater then zero")
  require(end < start, "Snake tail should be lesser then mouth")


}

object Snake {
  def getValidSnakes(snakes: List[Snake], max: Int): Map[Int, Int] = {
    snakes.map(s => {
      require(s.start <= max, "Snake mouth should be lesser then board square")
      s.start -> s.end
    })(collection.breakOut): Map[Int, Int]
  }

  def default = List(Snake(27, 1), Snake(21, 9), Snake(17, 4), Snake(19, 7))
}

case class Ladder(start: Int, end: Int) {
  require(start > 0, "Ladder start should be greater then zero")
  require(end > start, "Ladder start should be greater then  end")
}

object Ladder {
  def getValidLadders(ladders: List[Ladder], max: Int): Map[Int, Int] = {
    ladders.map(l => {
      require(l.end <= max, "Ladder end should be lesser then board square")
      l.start -> l.end
    })(collection.breakOut): Map[Int, Int]
  }

  def default = List(Ladder(11, 26), Ladder(3, 22), Ladder(5, 8), Ladder(20, 29))
}


case class SnakeAndLadders(length: Int = 6, width: Int = 5, snakes: List[Snake] = Snake.default, ladders: List[Ladder] = Ladder.default) {
  val max = length * width
  val snakeMap = Snake.getValidSnakes(snakes, max)
  val ladderMap = Ladder.getValidLadders(ladders, max)


  def move(p1: String, p2: String, pos1: Int, pos2: Int, turn: Int): Unit = {
    if (isWon(p1, pos1) || isWon(p2, pos2))
      return
    else {
      println(s"$p1 : at => $pos1 and $p2 : at => $pos2")
      simulateDice
      if (turn == 0) move(p1, p2, getNewPosition(p1, pos1), pos2, 1)
      else move(p1, p2, pos1, getNewPosition(p2, pos2), 0)
    }
  }

  def simulateDice = {
    val time = System.currentTimeMillis() + 1000
    print("\nRolling Dice")
    while (time > System.currentTimeMillis()) {
      Thread.sleep(100)
      print(".............")
    }
    println("")
  }

  def isWon(p: String, pos: Int): Boolean = {
    if (pos == max) {
      println(s"******************  Hurray $p  Won **************************")
      true
    } else false

  }

  def getNewPosition(p: String, pos: Int): Int = {
    val dice = (((Math.random() * 10) % 6) + 1).toInt
    print(s"$p got $dice ")
    if (dice + pos <= max) {
      print(s" moving $dice steps ahead \n")
      val newPos = pos + dice
      snakeMap get newPos match {
        case Some(bite) => {
          println(Console.RED + s"Snake bites : $p at : $newPos going down at : $bite" + Console.RESET)
          return bite
        }
        case None => ladderMap get newPos match {
          case Some(ladder) => {
            println(Console.GREEN + s"Wow  : $p get ladder at : $newPos going up at : $ladder" + Console.RESET)
            ladder
          }
          case None => newPos
        }
      }
    } else {
      print(Console.BLUE + " cannot jump ahead of max wait for next turn \n" + Console.RESET)
      pos
    }
  }

  def start(p1: String, p2: String = "Computer") {
    println("################################ Let's Start  ###############################")
    move(p1, p2, 0, 0, 0)
  }
}

object SnakeAndLadders {
  def main(args: Array[String]) {
    SnakeAndLadders().start("Joe")
    //SnakeAndLadders(10, 10, List(Snake(34, 1), Snake(25, 5), Snake(47, 9), Snake(65, 52), Snake(87, 57), Snake(99, 69), Snake(91, 61)), List(Ladder(3, 51), Ladder(6, 27), Ladder(20, 70), Ladder(36, 55), Ladder(63, 95), Ladder(68, 98))).start("Mark", "Steave")
  }
}

