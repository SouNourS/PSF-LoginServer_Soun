// Copyright (c) 2017 PSForever
package net.psforever.objects

import scala.collection.mutable

class VehicleDefinition(val objectId : Int, val name : String = "vehicle") {
  var maxHealth : Int = 100
  var maxShields : Int = 0
  val mountPointToSeat : mutable.HashMap[Int, Int] = mutable.HashMap()
  val seats : mutable.HashMap[Int, Seat] = mutable.HashMap[Int, Seat]()
  val weapons : mutable.ArrayBuffer[ToolDefinition] = mutable.ArrayBuffer[ToolDefinition]()
  val utilities : mutable.ArrayBuffer[Utility] = mutable.ArrayBuffer[Utility]()
  var trunkSize : InventoryTile = InventoryTile(1,1)
}

object VehicleDefinition {
  def apply(guid: Int, name : String) : VehicleDefinition = {
    new VehicleDefinition(guid, name)
  }

  //the following is just to make an example; the data is far from correct
  private val fury_weapon_system = ToolDefinition(0, "fury_weapon_system")
  fury_weapon_system.size = EquipmentSize.VEHICLEWEAPON

  final val Fury = VehicleDefinition(335, "Fury")
  Fury.maxHealth = 1000
  Fury.maxShields = 1000
  Fury.mountPointToSeat += 0 -> 0
  Fury.mountPointToSeat += 1 -> 0
  Fury.seats += 0 -> Seat()
  Fury.seats(0).setBailable(true)
  Fury.seats(0).setControlledWeapon(Some(0))
  Fury.weapons += fury_weapon_system
  Fury.trunkSize = new InventoryTile(8, 8)
}
