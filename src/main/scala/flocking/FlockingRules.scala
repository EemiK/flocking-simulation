package flocking

import flocking.GUI.members
import scala.collection.mutable.ListBuffer

object FlockingRules {

  var alignmentValue = 10
  var cohesionValue = 5
  var separationValue = 50
  var visionValue = 40
  var maxSpeed = 3

  /**
   * These help to set the interface values from the GUI sliders
   */

  def setAlignmentValue(value: Int) = this.alignmentValue = value

  def setSeparationValue(value: Int) = this.separationValue = value

  def setCohesionValue(value: Int) = this.cohesionValue = value

  def setVisionValue(value: Int) = this.visionValue = value

  def setMaxSpeedValue(value: Int) = this.maxSpeed = value

  /**
   * Returns all the Members surrounding the Member at the max distance of
   * radiusOfNeighborhood. Value will be collection Array[Member]
   */

  def neighbors(member: Member): ListBuffer[Member] = GUI.members.filter(i => member.place.distance(i.place) < visionValue * 20)

  /**
   * Takes a member as a parameter, calculates the neighbors and gathers all the speed: Vector2D components from the neighboring Members and
   * returns the average value. So easy math: (sum of all the speed) / (number of Members)
   */

  def averageSpeed(member: Member) = {
    val summed = (neighbors(member) += member).map(_.speed).reduce(_ + _)

    summed.divideBy(neighbors(member).length + 1)
  }

  /**
   * Takes one member as parameter, calculates neighbors for the member and gathers all the place: Vector2D
   * values and calculates the average position of the neighborhood. This unit will steer the member
   * towards the center of gravity and the agressiveness of the steering will be specified
   * by the values which the user chooses.
   */

  def centerOfGravity(member: Member) = {
    val summed = (neighbors(member) += member).map(_.place).reduce(_ + _)

    summed.divideBy(neighbors(member).length + 1)
  }

  /** Helper metho to calculate the sum of all the vectors in collection */
  def sum(vectors: ListBuffer[Vector2D]) = {
    if (vectors.nonEmpty) vectors.reduce(_ + _) else new Vector2D(0, 0)
  }

  /** Update method to keep up with the changes and update the GUI */

  def steering() = {

    (members.toArray).foreach(i => {

      /** Calculating the speed vector, so the direction and the movement speed for every member */
      i.speed = (i.speed +
        (FlockingRules.averageSpeed(i) - i.speed).multiplyBy(alignmentValue / 3000.0) +

        /** Alignment */
        (FlockingRules.centerOfGravity(i) - i.place).multiplyBy(cohesionValue / 3000.0) +

        /** Coheasion */
        FlockingRules.sum(FlockingRules.neighbors(i).map(n => i.place - n.place)).setMag(2).multiplyBy(separationValue / 1000.0) /** Separation */
        )

      .setMag(maxSpeed) /** Normalizing the vector to a certain magnitude */

      /** Checking if member is out of bounds, changes its position to the other side of the map so members don't disappear to the void */
      if (i.place.x + 50 * i.speed.x <= 0 || i.place.x + 50 * i.speed.x >= 1600) i.place = new Vector2D(1600 - i.place.x, 900 - i.place.y)
      if (i.place.y + 50 * i.speed.y <= 0 || i.place.y + 50 * i.speed.y >= 900) i.place = new Vector2D(1600 - i.place.x, 900 - i.place.y)

      /** After the direction has been determined rotate the member in GUI to face the direction */
      i.shape.setRotate((if (i.speed.x < 0) -270 else -90) + math.toDegrees(math.atan(i.speed.y / i.speed.x)))

      /** Adding the speed to the position */
      i.place = i.place + i.speed

      /** Changing members position in the GUI after its position has been updated */
      i.shape.setTranslateX(i.place.x)
      i.shape.setTranslateY(i.place.y)

    })
  }

}
