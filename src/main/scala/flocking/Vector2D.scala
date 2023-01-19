package flocking

/**
 * Describes a 2D mathematical vector
 */
case class Vector2D(x: Double, y: Double) {

  val length = math.hypot(x, y)
  val angle = math.atan2(y, x)

  /**
   * Add two vectors and return their sum vector
   */
  def +(other: Vector2D) = {
    Vector2D(x + other.x, y + other.y)
  }

  /**
   * Subtract other vector from this vector and return the outcome
   */

  def -(other: Vector2D) = {
    Vector2D(x - other.x, y - other.y)
  }

  /**
   * Divide the vector by wanted double value and return the outcome
   */

  def divideBy(n: Double) = {
    Vector2D(this.x / n, this.y / n)
  }

  /**
   * Multiply the vector by wanted double and return the outcome
   */

  def multiplyBy(n: Double) = {
    Vector2D(this.x * n, this.y * n)
  }

  /**
   * Calculate the distance between this.vector and other.vector
   */

  def distance(other: Vector2D) = {
    Math.sqrt(Math.pow(this.x - other.x, 2) + Math.pow(this.y - other.y, 2))
  }

  /**
   * Calculate the magnitude of the vector
   */

  def getMagnitude = math.sqrt(math.pow(this.x, 2) + math.pow(this.y, 2))

  /**
   * Normalize the vector and set a wanted magnitude for it
   */

  def setMag(n: Int): Vector2D = {
    var result = Vector2D(0, 0)
    if (this.getMagnitude == 0.0) return result
    else {
      val xi = x / getMagnitude * n
      val yi = y / getMagnitude * n
      result = Vector2D(xi, yi)
    }
    result
  }

}
