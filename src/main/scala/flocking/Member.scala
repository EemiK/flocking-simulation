package flocking

import scalafx.scene.CacheHint
import scalafx.scene.paint.Color
import scalafx.scene.shape.Polygon

case class Member(var speed: Vector2D, var place: Vector2D) {

  /**
   * Create the shape of the member which is a triangle by default but can be
   * easily changed by changing the points below
   */

  val shape = new Polygon()
  shape.getPoints.addAll(0, 15, 8, -8, -8, -8)

  /** Adding the three points to get the wanted triangle */
  shape.setCache(true)
  shape.setCacheHint(CacheHint.Speed)
  shape.setFill(Color.Green)

  /** Filling in the colour of the triangle which is linked to member */

}
