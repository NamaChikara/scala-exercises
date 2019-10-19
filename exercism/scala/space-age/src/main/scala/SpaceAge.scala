object SpaceAge {
  val SECONDS_TO_EARTH: Double = 31557600
  val EARTH_TO_MERCURY: Double = 0.2408467
  val EARTH_TO_VENUS: Double = 0.61519726
  val EARTH_TO_MARS: Double = 1.8808158
  val EARTH_TO_JUPITER: Double = 11.862615
  val EARTH_TO_SATURN: Double = 29.447498
  val EARTH_TO_URANUS: Double = 84.016846
  val EARTH_TO_NEPTUNE: Double = 164.79132
  // age is in seconds
  // set input to Double to avoid rounding
  def onEarth(age: Double): Double = {
    age / SECONDS_TO_EARTH
  }
  def onMercury(age: Double): Double = {
    onEarth(age) / EARTH_TO_MERCURY
  }
  def onVenus(age: Double): Double = {
    onEarth(age) / EARTH_TO_VENUS
  }
  def onMars(age: Double): Double = {
    onEarth(age) / EARTH_TO_MARS
  }
  def onJupiter(age: Double): Double = {
    onEarth(age) / EARTH_TO_JUPITER
  }
  def onSaturn(age: Double): Double = {
    onEarth(age) / EARTH_TO_SATURN
  }
  def onUranus(age: Double): Double = {
    onEarth(age) / EARTH_TO_URANUS
  }
  def onNeptune(age: Double): Double = {
    onEarth(age) / EARTH_TO_NEPTUNE
  }
}