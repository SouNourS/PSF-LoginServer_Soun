// Copyright (c) 2017 PSForever
package net.psforever.objects

import net.psforever.packet.game.{PlanetSideEmpire, PlanetSideGUID}

import scala.collection.mutable

//TODO borrowed from vehicle update; do not need it when integrating
object DriveState extends Enumeration {
  type Type = Value

  val Mobile = Value(0)
  val Undeployed = Value(1)
  val Unavailable = Value(2)
  val Deployed = Value(3)

  val State7 = Value(7)
}

class Vehicle(val guid : Int, private val vehicleDef : VehicleDefinition) extends PSGameObject {
  private var faction : PlanetSideEmpire.Value = PlanetSideEmpire.TR
  private var owner : Option[PlanetSideGUID] = None
  private var health : Int = 100
  private var shields : Int = 0
  private var deployed : DriveState.Value = DriveState.Mobile

  private val seats : mutable.HashMap[Int, Seat] = mutable.HashMap()
  private val weapons : mutable.ArrayBuffer[EquipmentSlot] = mutable.ArrayBuffer()
  private val utilities : mutable.ArrayBuffer[Utility] = mutable.ArrayBuffer()
  private val trunk : GridInventory = GridInventory(1, 1)

  Vehicle.loadVehicleDefinition(this)

  def getVehicleDefinition : VehicleDefinition = {
    this.vehicleDef
  }

  def getFaction : PlanetSideEmpire.Value = {
    this.faction
  }

  def setFaction(faction : PlanetSideEmpire.Value) : PlanetSideEmpire.Value = {
    this.faction = faction
    faction
  }

  def getOwner : Option[PlanetSideGUID] = {
    this.owner
  }

  def setOwner(owner : Option[PlanetSideGUID]) : Option[PlanetSideGUID] = {
    this.owner = owner
    owner
  }

  def getHealth : Int = {
    this.health
  }

  def setHealth(health : Int) : Int = {
    this.health = health
    health
  }

  def getMaxHealth : Int = {
    this.vehicleDef.maxHealth
  }

  def getShields : Int = {
    this.shields
  }

  def setShields(strength : Int) : Int = {
    this.shields = strength
    strength
  }

  def getMaxShield : Int = {
    this.vehicleDef.maxShields
  }

  def getDeployState : DriveState.Value = {
    this.deployed
  }

  def setDeployState(deploy : DriveState.Value) : DriveState.Value = {
    this.deployed = deploy
    deploy
  }

  def getSeatFromMount(mountPoint : Int) : Option[Seat] = {
    val seat : Option[Int] = this.vehicleDef.mountPointToSeat.get(mountPoint)
    if(seat.isDefined) {
      this.getSeat(seat.get)
    }
    else {
      None
    }
  }

  def getSeat(seatNumber : Int) : Option[Seat] = {
    if(seatNumber >= 0 && seatNumber < this.seats.size) {
      this.seats.get(seatNumber)
    }
    else {
      None
    }
  }

  def getWeapon(wepNumber : Int) : Option[Equipment] = {
    if(wepNumber >= 0 && wepNumber < this.weapons.length) {
      this.weapons(wepNumber).getEquipment
    }
    else {
      None
    }
  }

  def passengerInSeat(player : PlayerAvatar) : Option[Seat] = {
    var outSeat : Option[Seat] = None
    val GUID = PlanetSideGUID(player.guid)
    for(seat <- this.seats.values) {
      val occupant : Option[PlanetSideGUID] = seat.getOccupant
      if(occupant.isDefined && occupant.get == GUID) {
        outSeat = Some(seat)
      }
    }
    outSeat
  }

  def getWeaponControlledFromSeat(seatNumber : Int) : Option[Equipment] = {
    val seat : Option[Seat] = this.getSeat(seatNumber)
    var weapon : Option[Equipment] = None
    if(seat.isDefined) {
      val wepNumber : Option[Int] = seat.get.getControlledWeapon
      if(wepNumber.isDefined) {
        weapon = this.getWeapon(wepNumber.get)
      }
    }
    weapon
  }

  def getUtility(utilNumber : Int) : Option[Any] = {
    if(utilNumber >= 0 && utilNumber < this.utilities.size) {
      Some(this.utilities(utilNumber))
    }
    else {
      None
    }
  }

  def getTrunk : GridInventory = {
    this.trunk
  }
}

object Vehicle {
  def apply(guid : Int, vehicleDef : VehicleDefinition) : Vehicle = {
    new Vehicle(guid, vehicleDef)
  }

  def loadVehicleDefinition(vehicle : Vehicle) : Unit = {
    val vdef : VehicleDefinition = vehicle.getVehicleDefinition
    //general stuff
    vehicle.setHealth(vdef.maxHealth)
    //create weapons
    for(_ <- 0 to vdef.weapons.length) {
      val slot = EquipmentSlot(EquipmentSize.VEHICLEWEAPON)
      slot.setEquipment(new Tool(0, 0)) //TODO for "new Tool(0, 0)," the second param should be a ToolDefinition
      vehicle.weapons += slot
    }
    //create seats
    for((num, seat) <- vdef.seats) {
      val newSeat = Seat()
      newSeat.setFaction(seat.getFaction)
      newSeat.setBailable(seat.getBailable)
      newSeat.setArmorRestriction(seat.getArmorRestriction)
      newSeat.setControlledWeapon(seat.getControlledWeapon)
      vehicle.seats += num -> newSeat
    }
    //trunk
    vehicle.trunk.resize(vdef.trunkSize.width, vdef.trunkSize.height)
    //TODO utilies must be loaded on a case-by-case basis?
  }
}
