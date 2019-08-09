package me.amuxix.pattern

import me.amuxix.block.Block.Location
import me.amuxix.inventory.Item
import me.amuxix.pattern.RunePattern.isMirrored
import me.amuxix.runes.Rune
import me.amuxix.{Direction, Matrix4, Named, Player}

/**
  * Created by Amuxix on 01/12/2016.
  */
trait RunePattern[R <: Rune] extends Named {
  type RuneCreator = (Location, Player, Direction, Matrix4, Pattern) => R
  val runeCreator: RuneCreator
  val width: Option[Int] = None
  val verticality: Boolean = false
  val directional: Boolean = false
  val buildableOnCeiling: Boolean = true
  val activatesWith: PartialFunction[Option[Item], Boolean] = {
    case Some(item) => !item.material.isBlock
    case None       => true //Empty hand
  }

  val layers: List[BaseLayer]

  final lazy val activationLayer: Int = {
    val index = layers.indexWhere(_.isInstanceOf[ActivationLayer])
    if (index <= 0) {
      throw new Exception("Rune has no activation layer!")
    } else {
      index
    }
  }
  final lazy val finalWidth = width match {
    case None =>
      val sqrt = Math.sqrt(layers.head.elements.size)
      if (sqrt.isValidInt) {
        sqrt.toInt
      } else {
        throw new Exception("Rune has invalid width!")
      }
    case Some(width) if width % 2 == 0 => throw new Exception("Rune width needs to be odd!")
    case Some(width) => width
  }

  private val hasTwoMirroredAxis = layers.forall(isMirrored(_, finalWidth))
  private val elements = layers.map(_.toElementsArray(finalWidth))

  final lazy val pattern: Pattern = new Pattern(activationLayer, elements, hasTwoMirroredAxis, verticality, directional, buildableOnCeiling, activatesWith) {
    if (layers.exists(_.elements.size % finalWidth != 0)) {
      throw new Exception("At least a layer in the pattern does not form a rectangle!")
    }
    override protected[pattern] def createRune(
      center: Location,
      creator: Player,
      direction: Direction,
      rotation: Matrix4
    ): Rune = runeCreator(center, creator, direction, rotation, this)
  }
}

object RunePattern {
  def isMirrored(layer: BaseLayer, width: Int): Boolean = {
    val layerArray: Seq[Seq[Element]] = layer.toElementsArray(width)
    val height = layerArray.size
    (for {
      i <- 0 until Math.ceil(width / 2).toInt
      j <- 0 until Math.ceil(height / 2).toInt
      w <- 0 until width
      h <- 0 until height
    } yield layerArray(j)(w) == layerArray(width - j - 1)(w) && layerArray(h)(i) == layerArray(h)(height - i - 1)).forall(identity)
  }
}
