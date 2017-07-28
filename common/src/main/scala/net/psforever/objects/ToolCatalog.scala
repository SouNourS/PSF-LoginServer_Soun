// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import scala.collection.mutable

/**
  * A listing of all available tool types stored as ToolDefinitions alongside their GUID.
  * All references to the static values associated with the tool types should be made to their entries here.
  */
object ToolCatalog {
  private val catalog : mutable.HashMap[Int, ToolDefinition] = new mutable.HashMap[Int, ToolDefinition]

  var tool : ToolDefinition = _

  tool = ToolDefinition(132, "bank")
  tool.size = EquipmentSize.PISTOL
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.armor_canister
  tool.fireModes(0).magazineSize = 100
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.armor_canister
  tool.fireModes(1).magazineSize = 100
  tool.inventoryTileWidth = 3
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(140, "beamer")
  tool.size = EquipmentSize.PISTOL
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.energy_cell
  tool.fireModes(0).magazineSize = 16
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.energy_cell
  tool.fireModes(1).magazineSize = 16
  tool.inventoryTileWidth = 3
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(146, "bolt_driver")
  tool.size = EquipmentSize.RIFLE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.bolt
  tool.fireModes(0).magazineSize = 1
  tool.inventoryTileWidth = 6
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(175, "chainblade")
  tool.size = EquipmentSize.MELEE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.melee_ammo
  tool.fireModes(0).magazineSize = 1
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.melee_ammo
  tool.fireModes(1).magazineSize = 1
  //should never end up in inventory; will match glitch knife dimensions, however, if it does
  catalog += tool.guid -> tool

  tool = ToolDefinition(233, "cycler")
  tool.size = EquipmentSize.RIFLE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.bullet_9mm
  tool.fireModes(0).ammoTypes += Ammo.bullet_9mm_AP
  tool.fireModes(0).magazineSize = 50
  tool.inventoryTileWidth = 6
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(299, "flamethrower")
  tool.size = EquipmentSize.RIFLE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.flamethrower_ammo
  tool.fireModes(0).magazineSize = 100
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.flamethrower_ammo
  tool.fireModes(1).magazineSize = 100
  tool.inventoryTileWidth = 9
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(304, "flechette")
  tool.size = EquipmentSize.RIFLE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.shotgun_shell
  tool.fireModes(0).ammoTypes += Ammo.shotgun_shell_AP
  tool.fireModes(0).magazineSize = 96 // 12 * 8
  tool.inventoryTileWidth = 6
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(324, "forceblade")
  tool.size = EquipmentSize.MELEE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.melee_ammo
  tool.fireModes(0).magazineSize = 1
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.melee_ammo
  tool.fireModes(1).magazineSize = 1
  //should never end up in inventory; will match glitch knife dimensions, however, if it does
  catalog += tool.guid -> tool

  tool = ToolDefinition(336, "fury_weapon_systema")
  tool.size = EquipmentSize.VEHICLEWEAPON
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.rocket
  tool.fireModes(0).magazineSize = 2
  catalog += tool.guid -> tool

  tool = ToolDefinition(345, "gauss")
  tool.size = EquipmentSize.RIFLE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.bullet_9mm
  tool.fireModes(0).ammoTypes += Ammo.bullet_9mm_AP
  tool.fireModes(0).magazineSize = 30
  tool.inventoryTileWidth = 6
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(407, "ilc9")
  tool.size = EquipmentSize.PISTOL
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.bullet_9mm
  tool.fireModes(0).ammoTypes += Ammo.bullet_9mm_AP
  tool.fireModes(0).magazineSize = 30
  tool.inventoryTileWidth = 3
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(421, "katana")
  tool.size = EquipmentSize.MELEE
  tool.isConcurrentFeed = true
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.melee_ammo
  tool.fireModes(0).magazineSize = 1
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.melee_ammo
  tool.fireModes(1).magazineSize = 1
  //should never end up in inventory; will match glitch knife dimensions, however, if it does
  catalog += tool.guid -> tool

  tool = ToolDefinition(429, "lasher")
  tool.size = EquipmentSize.RIFLE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.energy_cell
  tool.fireModes(0).magazineSize = 35
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.energy_cell
  tool.fireModes(1).magazineSize = 35
  tool.inventoryTileWidth = 9
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(468, "magcutter")
  tool.size = EquipmentSize.MELEE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.melee_ammo
  tool.fireModes(0).magazineSize = 1
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.melee_ammo
  tool.fireModes(1).magazineSize = 1
  //should never end up in inventory; will match glitch knife dimensions, however, if it does
  catalog += tool.guid -> tool

  tool = ToolDefinition(531, "medicalapplicator")
  tool.size = EquipmentSize.PISTOL
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.health_canister
  tool.fireModes(0).magazineSize = 100
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.health_canister
  tool.fireModes(1).magazineSize = 100
  tool.inventoryTileWidth = 3
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(556, "mini_chaingun")
  tool.size = EquipmentSize.RIFLE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.bullet_9mm
  tool.fireModes(0).ammoTypes += Ammo.bullet_9mm_AP
  tool.fireModes(0).magazineSize = 100
  tool.inventoryTileWidth = 9
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(673, "phoenix") // Decimator
  tool.size = EquipmentSize.RIFLE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.phoenix_missile
  tool.fireModes(0).magazineSize = 3
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.phoenix_missile
  tool.fireModes(1).magazineSize = 3
  tool.inventoryTileWidth = 9
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(701, "pulsar")
  tool.size = EquipmentSize.RIFLE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.energy_cell
  tool.fireModes(0).magazineSize = 40
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.energy_cell
  tool.fireModes(1).magazineSize = 40
  tool.inventoryTileWidth = 6
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(714, "r_shotgun")
  tool.size = EquipmentSize.RIFLE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.shotgun_shell
  tool.fireModes(0).ammoTypes += Ammo.shotgun_shell_AP
  tool.fireModes(0).magazineSize = 128 // 16 * 8
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.shotgun_shell
  tool.fireModes(1).ammoTypes += Ammo.shotgun_shell_AP
  tool.fireModes(1).magazineSize = 128 // 16 * 8
  tool.inventoryTileWidth = 6
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(737, "rocklet")
  tool.size = EquipmentSize.RIFLE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.rocket
  tool.fireModes(0).ammoTypes += Ammo.frag_cartridge
  tool.fireModes(0).magazineSize = 6
  tool.fireModes += new FireModeDefinition
  tool.fireModes(1).ammoTypes += Ammo.rocket
  tool.fireModes(1).ammoTypes += Ammo.frag_cartridge
  tool.fireModes(1).magazineSize = 6
  tool.inventoryTileWidth = 6
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(845, "suppressor")
  tool.size = EquipmentSize.RIFLE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.bullet_9mm
  tool.fireModes(0).ammoTypes += Ammo.bullet_9mm_AP
  tool.fireModes(0).magazineSize = 25
  tool.inventoryTileWidth = 6
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  tool = ToolDefinition(1003, "winchester")
  tool.size = EquipmentSize.RIFLE
  tool.fireModes += new FireModeDefinition
  tool.fireModes(0).ammoTypes += Ammo.winchester_ammo
  tool.fireModes(0).magazineSize = 36
  tool.inventoryTileWidth = 6
  tool.inventoryTileHeight = 3
  catalog += tool.guid -> tool

  /**
    * Reference an existing exo-suit from the catalog.
    * @param guid the globally unique identifier
    * @return the exo-suit
    */
  def get(guid : Int) : Option[ToolDefinition] = {
    Option(catalog(guid))
  }
}
