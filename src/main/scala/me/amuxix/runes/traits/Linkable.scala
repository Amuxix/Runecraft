package me.amuxix.runes.traits

import cats.data.EitherT
import cats.effect.IO
import me.amuxix.block.Block
import me.amuxix.inventory.Item
import me.amuxix.material.Material
import me.amuxix.material.Material.Air
import me.amuxix.pattern.Signature
import me.amuxix.runes.Rune

/**
  * Created by Amuxix on 22/11/2016.
  */
/**
  * Defines a trait for runes that use signature blocks to link to other stuff: ie: Teleports, Waypoints
  */
trait Linkable extends Rune {
  val signatureBlocks: Seq[Block] = filteredRuneBlocksByElement(Signature)

  def signatureIsEmpty: Boolean = signatureBlocks.forall(_.material == Air)

  /**
    * Checks if the signature contains the material
    * @param material Material to be checked
    * @return true if signature contains a block with the given material
    */
  def signatureContains(material: Material): Boolean = {
    signatureBlocks.map(_.material).contains(material)
  }

  /**
    * Calculates the signature for this rune
    * @return The signature
    */
  def calculateSignature: Int = filteredRuneBlocksByElement(Signature).map(_.material.name).sorted.hashCode

  var signature: Int = calculateSignature

  /**
    * Checks whether the signature is valid for this rune and notifies player if it is not and why
    * @return true if signature is valid, false otherwise
    */
  def validateSignature: Option[String] = Option.when(signatureIsEmpty)("Signature is empty!")

  /**
    * This is where the rune effects when the rune is first activated go.
    * This must always be extended when overriding,
    */
  override def activate(activationItem: Option[Item]): EitherT[IO, String, Boolean] =
  for {
    _ <- EitherT.fromEither[IO](validateSignature.toLeft(()))
    _ = calculateSignature
    cancel <- super.activate(activationItem)
  } yield cancel
}
