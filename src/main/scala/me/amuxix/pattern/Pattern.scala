package me.amuxix.pattern

import me.amuxix.exceptions.InitializationException
import me.amuxix.logging.Logger.trace
import me.amuxix.material.{Material, NoEnergy, Block => MBlock}
import me.amuxix.pattern.matching.BoundingCube
import me.amuxix.runes.{Parameters, Rune}
import me.amuxix.{Block, Matrix4, Rotation, Vector3}

/**
  * Created by Amuxix on 21/11/2016.
  */

case class Layer(elements: Element*) {
  def toElementsArray(width: Int): Seq[Seq[Element]] = {
    elements.grouped(width).toSeq
  }
}
object ActivationLayer {
  def apply(elements: Element*) = new ActivationLayer(elements:_*)
}
class ActivationLayer(_elements: Element*) extends Layer(_elements:_*)

object Pattern {
  def isMirrored(layer: Layer, width: Int): Boolean = {
    val layerArray: Seq[Seq[Element]] = layer.toElementsArray(width)
    val height = layerArray.size
    (for {
      i <- 0 until Math.ceil(width / 2).toInt
      j <- 0 until Math.ceil(height / 2).toInt
      w <- 0 until width
      h <- 0 until height
    } yield layerArray(j)(w) == layerArray(width - j - 1)(w) && layerArray(h)(i) == layerArray(h)(height - i - 1)).forall(identity)
  }

  def apply[R <: Rune](creator: (Parameters, Pattern) => R, width: Int = 0, verticality: Boolean = false,
                       directional: Boolean = false, buildableOnCeiling: Boolean = true,
                       activatesWith: PartialFunction[Material, Boolean] = { case m if !m.isInstanceOf[MBlock] => true })
                      (layers: Layer*): Pattern = {
    val activationLayer = layers.indexWhere(_.isInstanceOf[ActivationLayer])
    val finalWidth: Int = if (width == 0) {
      val sqrt = Math.sqrt(layers.head.elements.size)
      if (sqrt.isValidInt) {
        sqrt.toInt
      } else {
        throw InitializationException("Rune has invalid width!")
      }
    } else {
      if (width % 2 == 0) {
        throw InitializationException("Rune width needs to be odd!")
      }
      width
    }
    if (layers.forall(_.elements.size % finalWidth == 0) == false) {
      throw InitializationException("At least a layer in the pattern does not form a rectangle!")
    }
    val hasTwoMirroredAxis = layers.forall(isMirrored(_, finalWidth))
    val elements = layers.map(_.toElementsArray(finalWidth))
    new Pattern(activationLayer, elements, hasTwoMirroredAxis, verticality, directional, buildableOnCeiling, activatesWith) {
      def createRune(parameters: Parameters): Rune = {
        creator(parameters, this)
      }
    }
  }
}


/**
  * Pattern for a rune
  * @param activationLayer Layer where the activation block is
  * @param elements The pattern itself, layer by layer
  * @param hasTwoMirroredAxis 0 - no mirroring, 1 - mirrored vertically horizontally, 2 - mirrored in both axis.
  * @param verticality Whether the rune can be made vertically
  * @param directional True when the rune has to be built in a certain direction
  * @param buildableOnCeiling True if this rune can have its layer order inverted to be activated looking from ground to ceiling
  */
abstract class Pattern private(activationLayer: Int, elements: Seq[Seq[Seq[Element]]], hasTwoMirroredAxis: Boolean, verticality: Boolean, directional: Boolean,
                       buildableOnCeiling: Boolean, val activatesWith: PartialFunction[Material, Boolean]) extends Ordered[Pattern] {
	/* IN GAME AXIS
		 *          Y axis
		 *          |
		 *          |     X axis
		 *          /----->  East
		 *        /
		 *      /Z axis
		 *    South
		 */
  private val height: Int = elements.length //Distance along the Y axis in the default orientation
  private val width: Int = elements.head.length //Distance along the X axis in the default orientation
  private val depth: Int = elements.head.head.length //Distance along the Z axis in the default orientation
	private val lowHeight: Int = activationLayer //Distance from bottom layer to the activation layer INCLUDING the activation layer
	private val highHeight: Int = height - activationLayer //Distance from the top layer to the activation layer EXCLUDING the activation layer
	val largestDimension: Int = (((lowHeight max (highHeight - 1)) * 2) + 1) max width max depth
  val volume: Int = height * width * depth
	val patternMaterials: Set[Material] = elements.flatten.flatten.collect { case m: Material => m }.toSet //Set of materials the rune contains

  /**
    * Attempts to create the rune, may fail.
    */
  protected[pattern] def createRune(parameters: Parameters): Rune

  /**
    * Looks for this pattern in the given bounding cube
    * @param boundingCube Cube to look for the pattern in
    * @return true if rune was found
    */
  def foundIn(boundingCube: BoundingCube): Option[Matrix4] = {
    val rotationMatrix = Matrix4.IDENTITY
    var possibleRotations = Seq(() => rotationMatrix)
    if (verticality) {
      possibleRotations :+= (() => rotationMatrix.rotateZ(90))
      possibleRotations :+= (() => rotationMatrix.rotateX(90))
    }
    if (height > 1) {
      if (verticality) {
        possibleRotations :+= (() => rotationMatrix.rotateZ(-90))
        possibleRotations :+= (() => rotationMatrix.rotateX(-90))
      }
      if (buildableOnCeiling) {
        possibleRotations :+= (() => rotationMatrix.rotateX(180))
      }
    }
    possibleRotations.find(f => matchHorizontalRotations(boundingCube, f()).isDefined).map(f => f())
  }

  /**
    * Checks if the center block can be the given material
    * @param centerMaterial Material we are matching against
    * @return true if this pattern can be made with the center of the given material
    */
  def centerCanBe(centerMaterial: Material): Boolean = {
    centerElement match {
      case material: Material if centerMaterial != material || centerMaterial.isInstanceOf[NoEnergy] => false
      case Tier if patternMaterials.contains(centerMaterial) => false
      case _ => true
    }
  }

  private def matchHorizontalRotations(boundingCube: BoundingCube, rotationMatrix: Matrix4): Option[Matrix4] = {
    var possibleRotations = Seq(() => rotationMatrix)
    if (directional == false) {
      if (hasTwoMirroredAxis == false) {
        possibleRotations :+= (() => rotationMatrix.rotateY(90))
        possibleRotations :+= (() => rotationMatrix.rotateY(180))
        possibleRotations :+= (() => rotationMatrix.rotateY(270))
      }
      if (hasTwoMirroredAxis == true && width != depth) {
        possibleRotations :+= (() => rotationMatrix.rotateY(90))
      }
    }
    possibleRotations.find(f => matchesRotation(boundingCube, f())).map(f => f())
  }

  private def matchesRotation(boundingCube: BoundingCube, rotationMatrix: Matrix4): Boolean = {
    var firstTier: Option[Material] = None
    var signature = Set.empty[Material]
    var key = Set.empty[Material]
    var notInRune = Set.empty[Material]
    val offsetVector: Vector3[Int] = offsetVectorFor(boundingCube)
    val rotation = Rotation(rotationMatrix, boundingCube.center)
    trace("Bounding cube dimension: " + boundingCube.dimension)
    trace(s"Pattern dimensions" + Vector3[Int](depth, height, width))
    trace("Offset Vector: " + offsetVector)
    trace("Rotation Matrix: " + rotationMatrix)
    //These offsets represent the difference in dimensions from the bounding cube to this pattern
    for {
      //This order ensures the first line checked is the northern most on the lowest layer
      layer <- 0 until height //Y
      block <- 0 until width //Z
      line <- 0 until depth //X
    } {
      val relativePosition: Vector3[Int] = Vector3(line, layer, block) + offsetVector
      val rotatedPosition: Vector3[Int] = rotation.rotate(relativePosition)
      trace("Relative Position: " + relativePosition)
      trace("Rotated Position: " + rotatedPosition)
      val blockMaterial: Material = boundingCube.blockAt(rotatedPosition).material
      trace("Block position: " + boundingCube.blockAt(rotatedPosition).location)
      trace("Block   Type: " + blockMaterial)
      trace("Pattern Type: " + elements(layer)(block)(line))
      //Having elements checked in layer > block > line makes the pattern top line be the northern most one
      elements(layer)(block)(line) match {
        case material: Material if blockMaterial != material =>
          //Material different from pattern
          trace("Material does not match")
          return false
        case Tier if patternMaterials.contains(blockMaterial) || blockMaterial.isInstanceOf[NoEnergy] =>
          trace("This block cannot be used as a tier material as its a material used by the rune or the material is unconsumable")
          return false
        case Tier =>
          if (firstTier.isDefined && blockMaterial != firstTier.get) {
            trace("All Tier blocks must be of same material")
            //All tier blocks must be the same material
            return false
          } else {
            firstTier = Some(blockMaterial)
            trace("Its a tier block")
          }
        case Signature if firstTier.isDefined && firstTier.get == blockMaterial =>
          trace("This block cannot be used as a signature as its a already material used by the rune or is being used as tier")
          return false
        case Key if firstTier.isDefined && firstTier.get == blockMaterial =>
          trace("This block cannot be used as a key as its a material already used by the rune or is being used as tier")
          return false
        case Signature => signature += blockMaterial
          trace("Its a Signature block")
        case Key => key += blockMaterial
          trace("Its a Key block")
        case NotInRune => notInRune += blockMaterial
          trace("Its a NotInRune block")
        case _ =>
          trace("Material matches")
      }
    }

    if (firstTier.isDefined) {
      val specialBlocks: Set[Material] = signature ++ key ++ notInRune
      if (specialBlocks contains firstTier.get) {
        //Signature, key and notInRune can't be the same as tier or specific materials
        return false
      }
      if (patternMaterials.exists((specialBlocks ++ firstTier).contains)) {
        //Checks if any of the special block is a pattern material, we don't want that to avoid possible missmatches
        return false
      }
    }
    trace("Pattern matches")
    true
  }

  /**
    * Applies the rotation given by the {@param rotationMatrix} to {@param point} about the {@param center} vector
    * @param center A vector to rotate the point about
    * @param rotationMatrix The matrix that defines the rotation
    * @param point Point to be rotated
    * @return A point rotated about the center with the given rotation matrix
    */
  def conjugateMatrix(center: Vector3[Int], rotationMatrix: Matrix4, point: Vector3[Int]): Vector3[Int] = {
    Matrix4.IDENTITY.translate(center) * rotationMatrix * Matrix4.IDENTITY.translate(center * -1) * point
  }

  /**
    * Calculates a vector that gives an offset to the position this pattern is inside the given bounding cube
    * @param boundingCube Bounding cube that contains the rune
    * @return OffsetVector
    */
  def offsetVectorFor(boundingCube: BoundingCube): Vector3[Int] = {
    Vector3(boundingCube.dimension - depth,  boundingCube.dimension - lowHeight, boundingCube.dimension - width) / 2
  }

  /**
    * Gets the blocks that belong to this pattern
    * @param boundingCube Cube this pattern fits in
    * @return Blocks that are inside the bounding cube that belong to this rune.
    */
  def runeBlocks(boundingCube: BoundingCube, rotation: Rotation): Array[Array[Array[Block]]] = {
    val offsetVector: Vector3[Int] = offsetVectorFor(boundingCube)
    Array.tabulate(depth, height, width) {
      case (x, y, z) =>
        boundingCube.blockAt(rotation.rotate(offsetVector + Vector3(x, y, z)))
    }
  }

  def nonSpecialBlockVectors: Seq[Vector3[Int]] = {
    for {
      layer <- 0 until height
      line <- 0 until depth
      block <- 0 until width
      el = elements(layer)(block)(line) if el.isInstanceOf[Material]
    } yield Vector3(line, layer, block)
  }

  def specialBlockVectors(element: Element): Seq[Vector3[Int]] = {
    for {
      layer <- 0 until height
      line <- 0 until depth
      block <- 0 until width
      el = elements(layer)(block)(line) if el == element
    } yield Vector3(line, layer, block)
  }

  def allRuneBlockVectors: Seq[Vector3[Int]] = {
    for {
      layer <- 0 until height
      line <- 0 until depth
      block <- 0 until width
      el = elements(layer)(block)(line) if el != NotInRune
    } yield Vector3(line, layer, block)
  }

	def centerElement: Element = elements(activationLayer)(width / 2)(depth / 2)

  override def compare(that: Pattern): Int = this.volume.compare(that.volume) * -1
}