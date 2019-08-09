package me.amuxix.material

import cats.data.NonEmptyList
import me.amuxix.Energy
import me.amuxix.bukkit.{Configuration, Recipes}
import me.amuxix.logging.Logger
import me.amuxix.material.Generic.Slab
import me.amuxix.material.Material._

/**
  * Created by Amuxix on 08/01/2017.
  */
object Recipe {
  def apply(choices: NonEmptyList[Material], result: Material) = new Recipe(NonEmptyList.one(choices), result, 1, requiresFuel = false)
  def apply(ingredient: Material, result: Material, craftedAmount: Double = 1, requiresFuel: Boolean = false): Recipe = new Recipe(NonEmptyList.one(NonEmptyList.one(ingredient)), result, craftedAmount, requiresFuel)

  val foodQuality: Double = 11/3 //Bread has 11 quality and takes 3 wheat to craft, so each quality is worth 3/11 * Wheat.
  private val quartzBlocks: NonEmptyList[Material] = NonEmptyList(QuartzBlock, List(QuartzPillar))
  private val redSandStoneBlocks: NonEmptyList[Material] = NonEmptyList(RedSandstone, List(CutRedSandstone))
  private val sandStoneBlocks: NonEmptyList[Material] = NonEmptyList(Sandstone, List(CutSandstone))
  private val stoneBrickBlocks: NonEmptyList[Material] = NonEmptyList(StoneBrick, List(MossyStoneBricks, CrackedStoneBricks))

  /**
    * The Recipe generated by this circumvents the circular dependency of Slabs and Chiseled blocks.
    */
  private def slabRecipe(blocks: NonEmptyList[Material], slab: Material with Slab): Recipe =
    Recipe(NonEmptyList(blocks, List(blocks, blocks)), slab, 6, requiresFuel = false)
  private val energyReceipes = Set(
    //region Ores
    Recipe(Coal, CoalOre, 1 / Configuration.oreMultiplier),
    Recipe(LapisLazuli, LapisLazuliOre, 1 / (6 * Configuration.oreMultiplier)),
    Recipe(Diamond, DiamondOre, Configuration.oreMultiplier),
    Recipe(Redstone, RedstoneOre, 1 / 4.5 * Configuration.oreMultiplier),
    Recipe(Emerald, EmeraldOre, 1 / Configuration.oreMultiplier),
    Recipe(Quartz, QuartzOre, 1 / Configuration.oreMultiplier),
    //endregion
    //region Misc
    Recipe(Redstone, RedstoneWire, 1 / Configuration.craftingMultiplier),
    Recipe(Sign, WallSign, 1 / Configuration.craftingMultiplier),
    Recipe(Torch, WallTorch, 1 / Configuration.craftingMultiplier),
    Recipe(RedstoneTorch, RedstoneWallTorch, 1 / Configuration.craftingMultiplier),
    Recipe(Dirt, Farmland),
    Recipe(GoldOre, SoulSand, 2),
    Recipe(String, Tripwire, 1 / Configuration.craftingMultiplier),
    Recipe(Dirt, GrassPath),
    Recipe(Ice, FrostedIce),
    Recipe(Water, BubbleColumn),
    Recipe(Pumpkin, CarvedPumpkin),
    Recipe(BeetrootSeeds, BeetrootPlantation),
    Recipe(Gravel, Flint, .5),
    Recipe(Anvil, ChippedAnvil, 1 / Configuration.craftingMultiplier * 2/3),
    Recipe(ChippedAnvil, DamagedAnvil, 1 / Configuration.craftingMultiplier * 1/2),
    slabRecipe(quartzBlocks, QuartzSlab),
    slabRecipe(redSandStoneBlocks, RedSandstoneSlab),
    slabRecipe(sandStoneBlocks, SandstoneSlab),
    slabRecipe(stoneBrickBlocks, StoneBrickSlab),
    //endregion
    //region Chainmail
    Recipe(IronHelmet, ChainmailHelmet, 1 / Configuration.chainmailMultiplier),
    Recipe(IronChestplate, ChainmailChestplate, 1 / Configuration.chainmailMultiplier),
    Recipe(IronLeggings, ChainmailLeggings, 1 / Configuration.chainmailMultiplier),
    Recipe(IronBoots, ChainmailBoots, 1 / Configuration.chainmailMultiplier),
    //endregion
    //region Buckets
    Recipe(NonEmptyList(Bucket, List(Water)), WaterBucket),
    Recipe(NonEmptyList(Bucket, List(Lava)), LavaBucket),
    Recipe(NonEmptyList(Bucket, List(Milk)), MilkBucket),
    Recipe(NonEmptyList(Bucket, List(Pufferfish)), PufferfishBucket),
    Recipe(NonEmptyList(Bucket, List(RawSalmon)), SalmonBucket),
    Recipe(NonEmptyList(Bucket, List(Cod)), CodBucket),
    Recipe(NonEmptyList(Bucket, List(TropicalFish)), TropicalFishBucket),
    //endregion
    //region Food
    Recipe(Wheat, RabbitStew, 1 / foodQuality * 22),
    Recipe(Wheat, CookedPorkchop, 1 / foodQuality * 20.8),
    Recipe(Wheat, CookedBeef, 1 / foodQuality * 20.8),
    Recipe(Wheat, GoldenCarrot, 1 / foodQuality * 20.4),
    Recipe(Wheat, Cake, 1 / foodQuality * 16.8),
    Recipe(Wheat, CookedMutton, 1 / foodQuality * 15.6),
    Recipe(Wheat, CookedSalmon, 1 / foodQuality * 15.6),
    Recipe(Wheat, GoldenApple, 1 / foodQuality * 13.6),
    Recipe(Wheat, EnchantedGoldenApple, 1 / foodQuality * 13.6),
    Recipe(Wheat, BeetrootSoup, 1 / foodQuality * 13.2),
    Recipe(Wheat, CookedChicken, 1 / foodQuality * 13.2),
    Recipe(Wheat, MushroomStew, 1 / foodQuality * 13.2),
    Recipe(Wheat, PumpkinPie, 1 / foodQuality * 12.8),
    Recipe(Wheat, BakedPotato, 1 / foodQuality * 11),
    Recipe(Wheat, Bread, 1 / foodQuality * 11),
    Recipe(Wheat, CookedCod, 1 / foodQuality * 11),
    Recipe(Wheat, CookedRabbit, 1 / foodQuality * 11),
    Recipe(Wheat, Carrots, 1 / foodQuality * 6.6),
    Recipe(Wheat, Apple, 1 / foodQuality * 6.4),
    Recipe(Wheat, ChorusFruit, 1 / foodQuality * 6.4),
    Recipe(Wheat, SpiderEye, 1 / foodQuality * 5.2),
    Recipe(Wheat, RawBeef, 1 / foodQuality * 4.8),
    Recipe(Wheat, RawPorkchop, 1 / foodQuality * 4.8),
    Recipe(Wheat, RawRabbit, 1 / foodQuality * 4.8),
    Recipe(Wheat, RottenFlesh, 1 / foodQuality * 4.8),
    Recipe(Wheat, MelonSlice, 1 / foodQuality * 3.2),
    Recipe(Wheat, PoisonousPotato, 1 / foodQuality * 3.2),
    Recipe(Wheat, RawChicken, 1 / foodQuality * 3.2),
    Recipe(Wheat, RawMutton, 1 / foodQuality * 3.2),
    Recipe(Wheat, Cookie, 1 / foodQuality * 2.4),
    Recipe(Wheat, Cod, 1 / foodQuality * 2.4),
    Recipe(Wheat, RawSalmon, 1 / foodQuality * 2.4),
    Recipe(Wheat, Beetroot, 1 / foodQuality * 2.2),
    Recipe(Wheat, Potatoes, 1 / foodQuality * 1.6),
    Recipe(Wheat, TropicalFish, 1 / foodQuality * 1.2),
    Recipe(Wheat, Pufferfish, 1 / foodQuality * 1.2),
    //endregion
    //region Food
    Recipe(AcaciaLog, StrippedAcaciaLog),
    Recipe(AcaciaWood, StrippedAcaciaWood),
    Recipe(BirchLog, StrippedBirchLog),
    Recipe(BirchWood, StrippedBirchWood),
    Recipe(DarkOakLog, StrippedDarkOakLog),
    Recipe(DarkOakWood, StrippedDarkOakWood),
    Recipe(JungleLog, StrippedJungleLog),
    Recipe(JungleWood, StrippedJungleWood),
    Recipe(OakLog, StrippedOakLog),
    Recipe(OakWood, StrippedOakWood),
    Recipe(SpruceLog, StrippedSpruceLog),
    Recipe(SpruceWood, StrippedSpruceWood),
    //endregion
    //region Banners
    Recipe(BlackBanner, BlackWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(BlueBanner, BlueWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(BrownBanner, BrownWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(CyanBanner, CyanWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(GrayBanner, GrayWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(GreenBanner, GreenWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(LightBlueBanner, LightBlueWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(LightGrayBanner, LightGrayWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(LimeBanner, LimeWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(MagentaBanner, MagentaWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(OrangeBanner, OrangeWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(PinkBanner, PinkWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(PurpleBanner, PurpleWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(RedBanner, RedWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(WhiteBanner, WhiteWallBanner, 1 / Configuration.craftingMultiplier),
    Recipe(YellowBanner, YellowWallBanner, 1 / Configuration.craftingMultiplier),
    //endregion
    //region Flower pots
    Recipe(NonEmptyList(FlowerPot, List(AcaciaSapling)), PottedAcaciaSapling),
    Recipe(NonEmptyList(FlowerPot, List(Allium)), PottedAllium),
    Recipe(NonEmptyList(FlowerPot, List(AzureBluet)), PottedAzureBluet),
    Recipe(NonEmptyList(FlowerPot, List(BirchSapling)), PottedBirchSapling),
    Recipe(NonEmptyList(FlowerPot, List(BlueOrchid)), PottedBlueOrchid),
    Recipe(NonEmptyList(FlowerPot, List(BrownMushroom)), PottedBrownMushroom),
    Recipe(NonEmptyList(FlowerPot, List(Cactus)), PottedCactus),
    Recipe(NonEmptyList(FlowerPot, List(Dandelion)), PottedDandelion),
    Recipe(NonEmptyList(FlowerPot, List(DarkOakSapling)), PottedDarkOakSapling),
    Recipe(NonEmptyList(FlowerPot, List(DeadBush)), PottedDeadBush),
    Recipe(NonEmptyList(FlowerPot, List(Fern)), PottedFern),
    Recipe(NonEmptyList(FlowerPot, List(JungleSapling)), PottedJungleSapling),
    Recipe(NonEmptyList(FlowerPot, List(OakSapling)), PottedOakSapling),
    Recipe(NonEmptyList(FlowerPot, List(OrangeTulip)), PottedOrangeTulip),
    Recipe(NonEmptyList(FlowerPot, List(OxeyeDaisy)), PottedOxeyeDaisy),
    Recipe(NonEmptyList(FlowerPot, List(PinkTulip)), PottedPinkTulip),
    Recipe(NonEmptyList(FlowerPot, List(Poppy)), PottedPoppy),
    Recipe(NonEmptyList(FlowerPot, List(RedMushroom)), PottedRedMushroom),
    Recipe(NonEmptyList(FlowerPot, List(RedTulip)), PottedRedTulip),
    Recipe(NonEmptyList(FlowerPot, List(SpruceSapling)), PottedSpruceSapling),
    Recipe(NonEmptyList(FlowerPot, List(WhiteTulip)), PottedWhiteTulip),
    //endregion
    //region Shulker Boxes
    Recipe(NonEmptyList(ShulkerBox, List(BoneMeal)), WhiteShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(OrangeDye)), OrangeShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(MagentaDye)), MagentaShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(LightBlueDye)), LightBlueShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(YellowDye)), YellowShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(LimeDye)), LimeShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(PinkDye)), PinkShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(GrayDye)), GrayShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(LightGrayDye)), LightGrayShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(CyanDye)), CyanShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(PurpleDye)), PurpleShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(LapisLazuli)), BlueShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(CocoaBeans)), BrownShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(GreenDye)), GreenShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(RoseBush)), RedShulkerBox),
    Recipe(NonEmptyList(ShulkerBox, List(InkSac)), BlackShulkerBox),
    //endregion
    //region Concrete
    Recipe(NonEmptyList(Water, List(WhiteConcretePowder)), WhiteConcrete),
    Recipe(NonEmptyList(Water, List(OrangeConcretePowder)), OrangeConcrete),
    Recipe(NonEmptyList(Water, List(MagentaConcretePowder)), MagentaConcrete),
    Recipe(NonEmptyList(Water, List(LightBlueConcretePowder)), LightBlueConcrete),
    Recipe(NonEmptyList(Water, List(YellowConcretePowder)), YellowConcrete),
    Recipe(NonEmptyList(Water, List(LimeConcretePowder)), LimeConcrete),
    Recipe(NonEmptyList(Water, List(PinkConcretePowder)), PinkConcrete),
    Recipe(NonEmptyList(Water, List(GrayConcretePowder)), GrayConcrete),
    Recipe(NonEmptyList(Water, List(LightGrayConcretePowder)), LightGrayConcrete),
    Recipe(NonEmptyList(Water, List(CyanConcretePowder)), CyanConcrete),
    Recipe(NonEmptyList(Water, List(PurpleConcretePowder)), PurpleConcrete),
    Recipe(NonEmptyList(Water, List(BlueConcretePowder)), BlueConcrete),
    Recipe(NonEmptyList(Water, List(BrownConcretePowder)), BrownConcrete),
    Recipe(NonEmptyList(Water, List(GreenConcretePowder)), GreenConcrete),
    Recipe(NonEmptyList(Water, List(RedConcretePowder)), RedConcrete),
    Recipe(NonEmptyList(Water, List(BlackConcretePowder)), BlackConcrete),
    //endregion
    //region 1.14 Smooth blocks
    Recipe(QuartzBlock, SmoothQuartz, requiresFuel = true),
    Recipe(RedSandstone, SmoothRedSandstone, requiresFuel = true),
    Recipe(Sandstone, SmoothSandstone, requiresFuel = true),
    Recipe(Stone, SmoothStone, requiresFuel = true),
    //endregion
  )

  val recipes: Set[Recipe] = Recipes.bukkitRecipes ++ energyReceipes
}

case class Recipe(ingredients: NonEmptyList[NonEmptyList[Material]], result: Material, craftedAmount: Double, requiresFuel: Boolean) {
  /**
    * Tries to update the energy of the resulting material of this recipe
    * @return True when energy was changed false otherwise.
    */
  def updateResultEnergy: Boolean = {
    //Logger.info(this)
    if (ingredients.exists(_.exists(_.energy.isEmpty))) {
      //No energy for some ingredients
      false
    } else {
      val ingredientsEnergy = ingredients.foldLeft(0.0) {
        case (total, possibilities) => total + possibilities.toList.flatMap(_.energy).map(_.value).min
      }
      val fuelEnergy: Energy = if (requiresFuel) Generic.cheapestSmeltEnergy else 0 Energy
      val newEnergy: Energy = ((ingredientsEnergy / craftedAmount) * Configuration.craftingMultiplier + fuelEnergy.value).ceil.toInt

      result.energy.filter(newEnergy >= _).fold {
        Logger.trace(s"${result.name}: ${result.energy.getOrElse("No Set")} -> $newEnergy")
        result.energy = newEnergy
        true
      }(_ => false)
    }
  }
}
