package flocking

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.control._
import scalafx.scene.input.MouseButton
import scalafx.scene.{PerspectiveCamera, Scene}

import java.io.{IOException, Reader}
import javafx.animation
import scala.collection.mutable.ListBuffer
import scala.util.Random

object GUI extends JFXApp {

  /**
   * GUI width and height, the area where the members can act
   */

  val windowWidth = 1600
  val windowHeight = 900

  /** Random generator to generate random numbers for locations */

  val random = new Random(System.currentTimeMillis())

  /**
   * GUI stage, camera, root, scene and contextMenu where are all the sliders.
   * Below also ListBuffer for all the current members in the simulation.
   */

  val stage1 = new PrimaryStage
  val camera = new PerspectiveCamera(false)

  val root = new scalafx.scene.Group
  val scene = new Scene(root, windowWidth, windowHeight, true)
  val contextMenu = new ContextMenu()
  var members: ListBuffer[Member] = ListBuffer()
  var ammountValue = 0

  /**
   * Setting the wanted ammount of members to start a new simulation
   */

  def setAmmountValue(value: Int) = this.ammountValue = value

  /**
   * Ammount of members to start the simulation with
   */

  class WrongFiletypeException(message: String) extends Exception(message)

  object Helpers {
    /**
     * Given a chunk header (an array of 5 chars) will return the size of this
     * chunks data.
     *
     * @param chunkHeader
     * a chunk header to process
     * @return the size of this chunk's data
     */
    def extractChunkSize(chunkHeader: Array[Char]): Int = {
      10 * chunkHeader(3).asDigit + chunkHeader(4).asDigit
    }

    /**
     * Given a chunk header (an array of 5 chars) will return the name of this
     * chunk as a 3-letter String.
     *
     * @param chunkHeader
     * a chunk header to process
     * @return the name of this chunk
     */
    def extractChunkName(chunkHeader: Array[Char]): String = {
      chunkHeader.take(3).mkString
    }

    /**
     * The read-method of the Reader class will occasionally read only part of
     * the characters that were requested. This method will repeatedly call read
     * to completely fill the given buffer. The size of the buffer tells the
     * algorithm how many bytes should be read.
     *
     * @param result
     * the result of the reading will be stored in this array
     * @param input
     * a character stream to read from
     * @throws IOException
     * @throws WrongFiletypeException if end of file is reached before
     *                                buffer is filled
     */
    def readFully(result: Array[Char], input: Reader) = {
      var cursor = 0
      while (cursor != result.length) {
        var numCharactersRead = input.read(result, cursor, result.length - cursor)
        // If the end of the file (EOF) is reached before the buffer is filled,
        // an exception is thrown.
        if (numCharactersRead == -1) {
          throw new WrongFiletypeException("Unexpected end of file.")
        }
        cursor += numCharactersRead
      }
    }
  }

  def loadStart(input: Reader) = {
    var header = new Array[Char](8)
    var date = new Array[Char](8)
    var headerChunk = new Array[Char](5)

    try {
      Helpers.readFully(header, input)
      Helpers.readFully(date, input)

      if (!header.mkString.startsWith("FLOCK")) {
        throw new WrongFiletypeException("Unknown file type")
      }

      val valueChunks = ListBuffer[String]()

      var finalFound = false
      while (!finalFound) {
        Helpers.readFully(headerChunk, input)
        Helpers.extractChunkName(headerChunk) match {
          case "ALG" => {
            val algValue = new Array[Char](Helpers.extractChunkSize(headerChunk)).drop(3).mkString.toInt
            FlockingRules.setAlignmentValue(algValue)
          }
          case "CHS" => {
            val chsValue = new Array[Char](Helpers.extractChunkSize(headerChunk)).drop(3).mkString.toInt
            FlockingRules.setCohesionValue(chsValue)
          }
          case "SPR" => {
            val sprValue = new Array[Char](Helpers.extractChunkSize(headerChunk)).drop(3).mkString.toInt
            FlockingRules.setSeparationValue(sprValue)
          }
          case "VSN" => {
            val vsnValue = new Array[Char](Helpers.extractChunkSize(headerChunk)).drop(3).mkString.toInt
            FlockingRules.setVisionValue(vsnValue)
          }
          case "MAX" => {
            val maxValue = new Array[Char](Helpers.extractChunkSize(headerChunk)).drop(3).mkString.toInt
            FlockingRules.setMaxSpeedValue(maxValue)
          }
          case "AMN" => {
            val amnValue = new Array[Char](Helpers.extractChunkSize(headerChunk)).drop(3).mkString.toInt
            setAmmountValue(amnValue)
          }
          case "FIN" => {
            finalFound = true
          }
        }
      }
    } catch {
      case e: IOException =>
        val flockExc = new WrongFiletypeException("Reading the file failed")
        flockExc.initCause(e)
        throw flockExc
    }
  }

  setAmmountValue(50)


  def start(n: Int) = {
    for (i <- 1 to n) {
      members += new Member(new Vector2D(random.between(-3.0, 3.0), random.between(-3.0, 3.0)),
        new Vector2D(random.between(0.0, 1024.0), random.between(0.0, 768)))
    }
  }

  /**
   * Helper method to create sliders with labels to GUI
   */

  def sliderWithLabel(slider: Slider, name: String, block: Int, min: Int, max: Int, start: Int, listener: javafx.beans.value.ChangeListener[Number]) = {
    slider.setBlockIncrement(block)
    slider.setMin(min)
    slider.setMax(max)
    slider.setValue(start)
    val label = new Label()
    label.setText(name + ": " + slider.valueProperty().getValue.intValue())

    val labelMenuItem = new CustomMenuItem(label)
    val sliderMenuItem = new CustomMenuItem(slider)

    slider.valueProperty().addListener(listener)
    slider.valueProperty().addListener((ov, old_val, new_val) => label.setText(name + new_val.intValue()))

    val result = ListBuffer(labelMenuItem, sliderMenuItem)
    result
  }

  /**
   * All the sliders created
   */

  val alignmentSlider = new Slider()
  val alignComponents = sliderWithLabel(alignmentSlider, "Alignment: ", 1, 0, 100, 10, (ov, old_val, new_val) => FlockingRules.setAlignmentValue(new_val.intValue()))

  val cohesionSlider = new Slider()
  val cohesionComponents = sliderWithLabel(cohesionSlider, "Cohesion: ", 1, 0, 100, 5, (ov, old_val, new_val) => FlockingRules.setCohesionValue(new_val.intValue()))

  val separationSlider = new Slider()
  val separationComponents = sliderWithLabel(separationSlider, "Separation: ", 1, 0, 100, 50, (ov, old_val, new_val) => FlockingRules.setSeparationValue(new_val.intValue()))

  val visionSlider = new Slider()
  val visionComponents = sliderWithLabel(visionSlider, "Vision: ", 1, 0, 100, 20, (ov, old_val, new_val) => FlockingRules.setVisionValue(new_val.intValue()))

  val ammountSlider = new Slider()
  val ammountComponents = sliderWithLabel(ammountSlider, "Ammount: ", 1, 0, 200, 50, (ov, old_val, new_val) => setAmmountValue(new_val.intValue()))

  val maxSpeedSlider = new Slider()
  val maxSpeedComponents = sliderWithLabel(maxSpeedSlider, "Max speed: ", 1, 1, 8, 3, (ov, old_val, new_val) => FlockingRules.setMaxSpeedValue(new_val.intValue()))

  /**
   * Sliders added to contextMenu
   */

  (alignComponents ++ cohesionComponents ++ separationComponents ++ visionComponents ++ ammountComponents ++ maxSpeedComponents).foreach(n => contextMenu.getItems.add(n))

  /** Button to empty the whole scene */

  val blank = new Button()
  blank.setText("Blank")
  contextMenu.getItems.add(new CustomMenuItem(blank))
  blank.setOnAction(e => members.foreach(n => scene.getChildren.remove(n.shape)))

  /** Button to empty the scene and add random amount of new members */

  val newStart = new Button()
  newStart.setText("New start")
  contextMenu.getItems.add(new CustomMenuItem(newStart))
  newStart.setOnAction(e => {
    members.foreach(n => scene.getChildren.clear())
    members.clear()
    start(ammountValue)
    members.foreach(n => scene.getChildren.add(n.shape))
  })

  /** If right click is clicke on the mouse, show the context menu with the slider
   * else add new member to the clicked position */

  scene.setOnMouseClicked(e => {
    if (e.getButton == MouseButton.Secondary.delegate) {
      contextMenu.show(stage1, e.getScreenX, e.getScreenY)
    }

    if (e.getButton == MouseButton.Primary.delegate) {
      val newMember = new Member(new Vector2D(random.between(-3.0, 3.0), random.between(-3.0, 3.0)), new Vector2D(e.getSceneX, e.getSceneY))
      members.addOne(newMember)
      root.getChildren.addAll(newMember.shape)
    }

  })

  /** Filling the scene with Balck or what ever color you want */

  scene.fill = scalafx.scene.paint.Color.Black

  /** Adding all the members and their postions to the scene */

  for (i <- members) {
    i.shape.setTranslateX(i.place.x)
    i.shape.setTranslateY(i.place.y)
    i.shape.setRotate(random.between(0.0, 180.0))
    scene.getChildren.addAll(i.shape)
  }

  /** Animation timer to handle all the action and updating */

  val t = new animation.AnimationTimer() {
    override def handle(now: Long): Unit = {
      FlockingRules.steering()
    }
  }

  /** Adding the scene to the stage */

  scene.setCamera(camera)
  stage1.setScene(scene)
  stage1.setTitle("Flocking")

  /** Starting the animation timer and painting the stage */

  t.start()
  stage1.show()

}
