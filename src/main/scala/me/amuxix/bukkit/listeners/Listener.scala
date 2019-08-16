package me.amuxix.bukkit.listeners

import java.util.UUID

import cats.data.EitherT
import cats.effect.IO
import me.amuxix.bukkit.Location.BukkitBlockPositionOps
import me.amuxix.bukkit.Player.BukkitPlayerOps
import me.amuxix.bukkit.inventory.Item
import me.amuxix.bukkit.inventory.Item.BukkitItemStackOps
import me.amuxix.bukkit.{Bukkit, Configuration}
import me.amuxix.pattern.matching.Matcher
import me.amuxix.position.BlockPosition
import me.amuxix.{IntegrityMonitor, Player}
import org.bukkit.entity.EntityType.DROPPED_ITEM
import org.bukkit.entity.{Item => BItem}
import org.bukkit.event.Event.Result
import org.bukkit.event.EventHandler
import org.bukkit.event.EventPriority._
import org.bukkit.event.block.Action.RIGHT_CLICK_BLOCK
import org.bukkit.event.entity.EntityDamageEvent
import org.bukkit.event.entity.EntityDamageEvent.DamageCause._
import org.bukkit.event.player.PlayerInteractEvent
import org.bukkit.inventory.EquipmentSlot.{HAND, OFF_HAND}

import scala.jdk.CollectionConverters._

object Listener extends org.bukkit.event.Listener {
  /**
    * This maps each player to last location they activated a rune at.
    * This is used to cancel off hand interact events.
    */
  var lastActivatedRune: Map[UUID, BlockPosition] = Map.empty

  @EventHandler(priority = LOWEST)
  def onPlayerInteract(event: PlayerInteractEvent): Unit = {
    //This gets fired Twice in a row, once for main hand and one for the off hand
    if (event.getAction == RIGHT_CLICK_BLOCK) {
      val clickedBlockLocation: BlockPosition = event.getClickedBlock.getLocation.aetherize
      val player: Player = event.getPlayer.aetherize
      if (event.getHand == OFF_HAND) {
        //If the player activates a rune with the main hand and places a block with the offhand we cancel the block placement.
        val cancel = lastActivatedRune.get(player.uuid).contains(clickedBlockLocation)
        event.setCancelled(cancel)
        lastActivatedRune -= player.uuid
      } else {
        val itemInHand: Option[Item] = Option(event.getItem).map(_.aetherize)
        if (event.getHand == HAND) {
          (if (IntegrityMonitor.persistentRunes.contains(clickedBlockLocation) && itemInHand.forall(_.material.isSolid == false)) {
            //There is a rune at this location, update it.
            lastActivatedRune += player.uuid -> clickedBlockLocation
            IntegrityMonitor.persistentRunes(clickedBlockLocation).update(player)
          } else {
            //Look for new runes
            Matcher.lookForRunesAt(clickedBlockLocation, player, event.getBlockFace, itemInHand).map(_.activate(itemInHand)) match {
              case None => EitherT.rightT[IO, String](event.useInteractedBlock() == Result.DENY)
              case Some(rune) =>
                lastActivatedRune += player.uuid -> clickedBlockLocation
                rune
            }
          }).value.flatMap {
            case Right(cancel) => IO(event.setCancelled(cancel))
            case Left(error) => player.notifyError(error)
          }.unsafeRunSync()
        }
      }
    }
  }

  @EventHandler(priority = NORMAL, ignoreCancelled = true)
  def onEntityDamageEvent(event: EntityDamageEvent): Unit = {
    if (event.getEntityType == DROPPED_ITEM && (event.getCause == FIRE || event.getCause == LAVA)) {
      val entity = event.getEntity
      val item = entity.asInstanceOf[BItem].getItemStack.aetherize
      if (item.material.hasEnergy) {
        val itemPosition = entity.getLocation.aetherize
        findClosestPlayerTo(itemPosition, Configuration.maxBurnDistance).foreach { player =>
          player.addMaximumEnergyFrom(item.consume).value.unsafeRunSync()
        }
      }
    }

  }

  private def findClosestPlayerTo(where: BlockPosition, maxDistance: Double): Option[Player] =
    Bukkit.server.getOnlinePlayers.asScala
      .foldLeft(Option.empty[(Player, Double)]) { case (None, player) =>
        val distance = player.getLocation.aetherize.distance(where)
        Option.when(distance <= maxDistance)((player.aetherize, distance))
      case (closest @ Some((_, closestPlayerDistance)), player) =>
        val distance = player.getLocation.aetherize.distance(where)
        if (distance < closestPlayerDistance) {
          Some((player.aetherize, distance))
        } else {
          closest
        }
      }.map(_._1)
}
