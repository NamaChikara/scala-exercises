import scala.math.cos
import scala.math.sin
import scala.math.Pi

object Bearing {
  val East: Int = 0
  val North: Int = 90
  val West: Int = 180
  val South: Int = 270
}

case class Space (val bearing: Int, val coordinates: (Int, Int))

case class Robot (val direction: Int, val position: (Int, Int)) {

  def applyChanges(orders: String, state: Space): Space = {

    if (orders.length == 0) {
      state
    } else {
      val temp_state = orders.head.toString match {
        case "A" => advance(state)
        case "L" => turnLeft(state)
        case "R" => turnRight(state)
        case _   => state
      }
      applyChanges(orders.tail, temp_state)
    }

  }

  def simulate(orders: String, state: Space = Space(direction, position)): Robot = {

    val new_state = applyChanges(orders, state)

    Robot(new_state.bearing, new_state.coordinates)

  }

  def changeBearing(current: Int, diff: Int): Int = {

    if (current + diff < 0) {
      current + diff + 360
    } else {
      (current + diff) % 360
    }

  }

  def advance: Space = {
    advance(Space(direction, position))
  }
  def advance(state: Space): Space = {
    val x_pos = state.coordinates._1 + cos(state.bearing * Pi / 180).toInt
    val y_pos = state.coordinates._2 + sin(state.bearing * Pi / 180).toInt
    Space(state.bearing, (x_pos, y_pos))
  }

  def turnLeft: Space = {
    turnLeft(Space(direction, position))
  }
  def turnLeft(data: Space): Space = {
    Space(changeBearing(data.bearing, 90), data.coordinates)
  }

  def turnRight: Space = {
    turnRight(Space(direction, position))
  }
  def turnRight(data: Space): Space = {
    Space(changeBearing(data.bearing, -90), data.coordinates)
  }

}

object Robot {

  def apply(direction: Int, position: (Int, Int)): Robot = {
    new Robot(direction, position)
  }

}


// https://alvinalexander.com/scala/how-to-create-scala-object-instances-without-new-apply-case-class
// https://stackoverflow.com/questions/22103257/scala-class-and-case-class-comparison
// https://stackoverflow.com/questions/1020653/how-can-you-do-anything-useful-without-mutable-state

// Need to look at: https://dzone.com/articles/secret-powers-foldleft-scala