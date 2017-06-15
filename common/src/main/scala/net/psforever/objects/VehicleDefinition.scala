// Copyright (c) 2017 PSForever
package net.psforever.objects

import scala.collection.mutable

/**
  * An object definition system used to construct and retain the parameters of various vehicles.
  * @param objectId the object id the is associated with this sort of `Vehicle`
  * @param name the common name of the vehicle;
  *             defaults to "vehicle"
  */
class VehicleDefinition(val objectId : Int, val name : String = "vehicle") {
  var maxHealth : Int = 100
  var maxShields : Int = 0
  /* key - seat index, value - seat object */
  val seats : mutable.HashMap[Int, Seat] = mutable.HashMap[Int, Seat]()
  /* key - entry point index, value - seat index */
  val mountPoints : mutable.HashMap[Int, Int] = mutable.HashMap()
  /* key - seat index (where this weapon attaches during object construction), value - the weapon on an EquipmentSlot */
  val weapons : mutable.HashMap[Int, ToolDefinition] = mutable.HashMap[Int, ToolDefinition]()
  val utilities : mutable.ArrayBuffer[Utility] = mutable.ArrayBuffer[Utility]()
  var trunkSize : InventoryTile = InventoryTile(1,1)
  var trunkOffset: Int = 0
}

object VehicleDefinition {
  def apply(guid: Int, name : String) : VehicleDefinition = {
    new VehicleDefinition(guid, name)
  }

  //the following is just to make an example; the data is not expected to be correct
  private val fury_weapon_system = ToolDefinition(0, "fury_weapon_system")
  fury_weapon_system.size = EquipmentSize.VEHICLEWEAPON

  final val Fury = VehicleDefinition(335, "Fury")
  Fury.maxHealth = 1000
  Fury.maxShields = 1000
  Fury.seats += 0 -> Seat()
  Fury.seats(0).setBailable(true)
  Fury.seats(0).setControlledWeapon(Some(1))
  Fury.mountPoints += 0 -> 0
  Fury.mountPoints += 2 -> 0
  Fury.weapons += 1 -> fury_weapon_system
  Fury.trunkSize = new InventoryTile(8, 8)
  Fury.trunkOffset = 2
}
