import flocking.{Member, Vector2D}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

package object Tests {

  class Vector2DTests extends AnyFunSuite {

    test("Vector should be correct") {
      assert(new Vector2D(2, 1) + new Vector2D(0, 3) == new Vector2D(2, 4))
    }

    test("Vector should be correct") {
      assert(new Vector2D(10, 5) - new Vector2D(1, 2) == new Vector2D(9, 3))
    }

    test("Vector should be correct") {
      assert(new Vector2D(4, 6).divideBy(2) == new Vector2D(2, 3))
    }

    test("Vector should be correct") {
      assert(new Vector2D(4, 6).multiplyBy(2) == new Vector2D(8, 12))
    }

    test("Distance should be correct") {
      assert(new Vector2D(4, 6).distance(new Vector2D(4, 10)) == 4)
    }

    test("Madnitude isn't correct") {
      assert(new Vector2D(0, 2).getMagnitude == 1)
    }

    test("Magnitude isn't correct") {
      assert(new Vector2D(3, 0).setMag(1) == new Vector2D(1, 0))
    }

  }

  class GUITests extends AnyFunSuite {

    test("AmmountValue isn't correct") {
      flocking.GUI.setAmmountValue(11)
      assert(flocking.GUI.ammountValue == 11)
    }

    test("Starting ammount of members isn't correct") {
      flocking.GUI.start(10)
      assert(flocking.GUI.members.size == 10)
    }

  }

  class FlockingRulesTests extends AnyFunSuite {

    test("AmmountValue isn't correct") {
      flocking.FlockingRules.setAlignmentValue(5)
      assert(flocking.FlockingRules.alignmentValue == 5)
    }

    test("CohesionValue isn't correct") {
      flocking.FlockingRules.setCohesionValue(7)
      assert(flocking.FlockingRules.cohesionValue == 7)
    }

    test("SeparationValue isn't correct") {
      flocking.FlockingRules.setSeparationValue(17)
      assert(flocking.FlockingRules.alignmentValue == 17)
    }

    test("VisionValue isn't correct") {
      flocking.FlockingRules.setVisionValue(22)
      assert(flocking.FlockingRules.visionValue == 22)
    }

    test("MaxSpeedValue isn't correct") {
      flocking.FlockingRules.setMaxSpeedValue(3)
      assert(flocking.FlockingRules.maxSpeed == 3)
    }

    test("Neighbors are not correct") {
      flocking.FlockingRules.visionValue = 1
      val one = new Member(Vector2D(1, 2), Vector2D(1, 1))
      val two = new Member(Vector2D(1, 1), Vector2D(1, 10))
      val three = new Member(Vector2D(1, 3), Vector2D(1, 15))
      val four = new Member(Vector2D(1, 2), Vector2D(1, 30))
      assert(flocking.FlockingRules.neighbors(one) == ListBuffer(two, three))
    }

    test("Average speed isn't correct are not correct") {
      flocking.FlockingRules.visionValue = 1
      val one = new Member(Vector2D(1, 2), Vector2D(1, 1))
      val two = new Member(Vector2D(1, 1), Vector2D(1, 10))
      val three = new Member(Vector2D(1, 3), Vector2D(1, 15))
      val four = new Member(Vector2D(1, 2), Vector2D(1, 30))
      assert(flocking.FlockingRules.averageSpeed(one) == new Vector2D(1, 2))
    }

    test("Average speed isn't correct are not correct") {
      flocking.FlockingRules.visionValue = 1
      val one = new Member(Vector2D(1, 2), Vector2D(1, 5))
      val two = new Member(Vector2D(1, 1), Vector2D(1, 10))
      val three = new Member(Vector2D(1, 3), Vector2D(1, 15))
      val four = new Member(Vector2D(1, 2), Vector2D(1, 30))
      assert(flocking.FlockingRules.centerOfGravity(one) == new Vector2D(1, 10))
    }

    test("The summed vector isn't correct") {
      assert(flocking.FlockingRules.sum(ListBuffer(new Vector2D(1, 1), new Vector2D(2, 2))) == new Vector2D(3, 3))
    }

  }

}
