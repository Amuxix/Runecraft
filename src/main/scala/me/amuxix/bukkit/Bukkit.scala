package me.amuxix.bukkit

import better.files._
import cats.effect.IO
import me.amuxix._
import me.amuxix.bukkit.World.BukkitWorldOps
import me.amuxix.bukkit.listeners._
import me.amuxix.serialization.Persistable
import org.bukkit.Bukkit.getScheduler
import org.bukkit.configuration.file.FileConfiguration
import org.bukkit.event.Event
import org.bukkit.plugin.java.JavaPlugin
import org.bukkit.scheduler.BukkitRunnable
import org.bukkit.{Server, Bukkit => BukkitAPI}

import scala.jdk.CollectionConverters._

object Bukkit {
  private[bukkit] var server: Server = _
  private[bukkit] var config: FileConfiguration = _
  private[bukkit] var self: JavaPlugin = _

  def callEvent(event: Event): Unit = server.getPluginManager.callEvent(event)

  def runTaskSync(task: IO[Unit]): Unit = {
    new BukkitRunnable {
      override def run(): Unit = task.unsafeRunSync()
      runTask(self)
    }
  }

  def runTaskLater(task: IO[Unit], delay: Int): Unit = {
    new BukkitRunnable {
      override def run(): Unit = task.unsafeRunSync()
      runTaskLater(self, delay)
    }
  }

  /**
    * Setups a repeating task
    * @param task Function that generates the task to run
    * @param period Period in server ticks of the task
    * @return Task id number (-1 if scheduling failed)
    */
  def setupRepeatingTask(task: IO[Unit], period: Int): Int = {
    val runnable: Runnable = () => task.unsafeRunSync()
    getScheduler.scheduleSyncRepeatingTask(self, runnable, 0L, period.toLong)
  }

  def cancelRepeatingTask(taskNumber: Int) = getScheduler.cancelTask(taskNumber)
}

/**
  * Created by Amuxix on 21/11/2016.
  */
class Bukkit extends JavaPlugin {
	/**
	  * This register this file as a listener to all of bukkit events.
	  */
	override def onEnable(): Unit = {
    Bukkit.server = getServer
    Bukkit.self = this
    Bukkit.config = getConfig

    val registerEvents = IO {
      BukkitAPI.getPluginManager.registerEvents(Listener, this)
      BukkitAPI.getPluginManager.registerEvents(EnchantListener, this)
      BukkitAPI.getPluginManager.registerEvents(IntegrityListener, this)
    }

    Aethercraft.load(
      logger = getLogger,
      version = getDescription.getFullName,
      worlds = getServer.getWorlds.asScala.toList.map(_.aetherize),
      dataFolder = getDataFolder.toScala,
      saveDefaultConfig = IO(saveDefaultConfig()),
      registerEvents = registerEvents
    )

	}

  override def onDisable(): Unit = {
    Bukkit.cancelRepeatingTask(Aethercraft.builderTaskId)
    Persistable.saveEverything.unsafeRunSync()
  }
}