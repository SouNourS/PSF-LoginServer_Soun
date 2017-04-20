// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import net.psforever.types.ExoSuitType

import scala.collection.mutable

/**
  * A listing of all available exo-suit types by their GUID.
  * All references to the static values associated with the suit types should be made to their entries here.
  */
object ExoSuitCatalog {
  /**
    * A mapping of all of the hard defined exo-suits.
    */
  private val catalog : mutable.HashMap[ExoSuitType.Value, ExoSuit] = mutable.HashMap[ExoSuitType.Value, ExoSuit]()

  var armor : ExoSuit = _

  armor = ExoSuit(ExoSuitType.Agile)
  armor.name = "agile exo-suit"
  armor.maxArmor = 100
  armor.inventoryWidth = 9
  armor.inventoryHeight = 9
  armor.holsterTypes(0) = EquipmentSize.PISTOL
  armor.holsterTypes(1) = EquipmentSize.PISTOL
  armor.holsterTypes(2) = EquipmentSize.RIFLE
  armor.holsterTypes(4) = EquipmentSize.MELEE
  catalog += armor.guid -> armor

  armor = ExoSuit(ExoSuitType.Reinforced)
  armor.name = "reinforced exo-suit"
  armor.permission = 1 //TODO "reinforced exo-suit" certification needed
  armor.maxArmor = 200
  armor.inventoryWidth = 12
  armor.inventoryHeight = 9
  armor.holsterTypes(0) = EquipmentSize.PISTOL
  armor.holsterTypes(1) = EquipmentSize.PISTOL
  armor.holsterTypes(2) = EquipmentSize.RIFLE
  armor.holsterTypes(3) = EquipmentSize.RIFLE
  armor.holsterTypes(4) = EquipmentSize.MELEE
  catalog += armor.guid -> armor

  armor = ExoSuit(ExoSuitType.MAX)
  armor.name = "mechanized assault exo-suit"
  armor.permission = 1 //TODO a "max" certification needed for suit, max weapons are permitted by other certification specifics
  armor.maxArmor = 650
  armor.inventoryWidth = 16
  armor.inventoryHeight = 16
  armor.holsterTypes(0) = EquipmentSize.MAX // TODO how to handle this?
  catalog += armor.guid -> armor

  armor = ExoSuit(ExoSuitType.Infiltration)
  armor.name = "infiltration suit"
  armor.permission = 1 //TODO "infiltration suit" certification needed
  armor.maxArmor = 0
  armor.inventoryWidth = 6
  armor.inventoryHeight = 6
  armor.holsterTypes(0) = EquipmentSize.PISTOL // TODO check that it is not pistol slot 1
  armor.holsterTypes(4) = EquipmentSize.MELEE
  catalog += armor.guid -> armor

  armor = ExoSuit(ExoSuitType.Standard)
  armor.name = "standard exo-suit"
  armor.maxArmor = 50
  armor.inventoryWidth = 9
  armor.inventoryHeight = 6
  armor.holsterTypes(0) = EquipmentSize.PISTOL
  armor.holsterTypes(2) = EquipmentSize.RIFLE
  armor.holsterTypes(4) = EquipmentSize.MELEE
  catalog += armor.guid -> armor

  /**
    * Reference an existing exo-suit from the catalog.
    * @param guid the globally unique identifier
    * @return the exo-suit
    */
  def get(guid : ExoSuitType.Value) : Option[ExoSuit] = {
    catalog.get(guid)
  }
}
