package me.amuxix.bukkit.inventory

import me.amuxix.{Aetherizeable, inventory}
import me.amuxix.bukkit.inventory.Item.BukkitItemStackOps
import org.bukkit.inventory.{PlayerInventory => BukkitPlayerInventory}

object PlayerInventory {
  implicit class BukkitInventoryOps(inventory: BukkitPlayerInventory) extends Aetherizeable[PlayerInventory] {
    override def aetherize: PlayerInventory = new PlayerInventory(inventory)
  }
}

private[bukkit] class PlayerInventory(inv: BukkitPlayerInventory) extends Inventory(inv) with inventory.PlayerInventory {
  override def helmet: Option[Item] = Option(inv.getHelmet).map(_.aetherize)

  override def itemInMainHand: Item = inv.getItemInMainHand.aetherize

  override def itemInOffHand: Item = inv.getItemInOffHand.aetherize
}
