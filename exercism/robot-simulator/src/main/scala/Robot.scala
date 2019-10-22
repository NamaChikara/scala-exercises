import scala.math

object Bearing {
  val North: Int = 0
  val East: Int = 90
  val South: Int = 180
  val West: Int = 270
}

case class Space (val bearing: Int, val coordinates: (Int, Int))

case class Robot (val direction: Int, val position: (Int, Int)) {

  def simulate: Unit = {

  }

  def changeBearing(current: Int, diff: Int): Int = {

    if (current + diff < 0) {
      current + diff + 360
    } else {
      (current + diff) % 360
    }
  }

  def advance: Space = {
    val x_pos = position._1 + scala.math.cos((direction + 90) * scala.math.Pi / 180).floor.toInt
    val y_pos = position._2 + scala.math.sin((direction + 90) * scala.math.Pi / 180).floor.toInt
    Space(direction, (x_pos, y_pos))
  }
  def turnLeft: Space = {
    Space(changeBearing(direction, -90), position)
  }
  def turnLeft(data: Space): Space = {
    Space(data.bearing + 90, data.coordinates)
  }
  def turnRight: Space = {
    Space(changeBearing(current = direction, diff = 90), position)
  }
  def turnRight(data: Space): Space = {
    Space(data.bearing + 90, data.coordinates)
  }

}

object Robot {

  def apply(direction: Int, position: (Int, Int)): Robot = {
    new Robot(direction, position)
  }

}


// https://alvinalexander.com/scala/how-to-create-scala-object-instances-without-new-apply-case-class
// https://stackoverflow.com/questions/22103257/scala-class-and-case-class-comparison