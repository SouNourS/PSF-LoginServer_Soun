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

/**
  * The server-side support object that represents a vehicle.<br>
  * <br>
  * All infantry seating, all mounted weapons, and the trunk space are considered part of the same index hierarchy.
  * Generally, all seating is declared first - the driver and passengers and and gunners.
  * Following that are the mounted weapons and other utilities.
  * Trunk space starts being indexed afterwards.
  * The first seat is always the op;erator (driver/pilot).
  * "Passengers" are seats that are not the operator and are not in control of a mounted weapon.
  * "Gunners" are seats that are not the operator and ARE in control of a mounted weapon.
  * (The operator can be in control of a weapon - that is the whole point of a turret.)<br>
  * <br>
  * Having said all that, to keep it simple, infantry seating, mounted weapons, and utilities are stored in separate `HashMap`s.
  * @param guid the vehicle's globally unique identifier
  * @param vehicleDef the vehicle's definition entry'
  *                   stores and unloads pertinent information about the `Vehicle`'s configuration;
  *                   used in the initialization process (`loadVehicleDefinition`)
  */
class Vehicle(val guid : Int, private val vehicleDef : VehicleDefinition) extends PSGameObject {
  private var faction : PlanetSideEmpire.Value = PlanetSideEmpire.TR
  private var owner : Option[PlanetSideGUID] = None
  private var health : Int = 1
  private var shields : Int = 0
  private var deployed : DriveState.Value = DriveState.Mobile
  private var decal : Int = 0
  private var trunkLockState : VehicleLockState.Value = VehicleLockState.Locked

  private val seats : mutable.HashMap[Int, Seat] = mutable.HashMap()
  private val weapons : mutable.HashMap[Int, EquipmentSlot] = mutable.HashMap()
  private val utilities : mutable.ArrayBuffer[Utility] = mutable.ArrayBuffer()
  private var trunk : Trunk = Trunk(1, 1, 0)

  //init
  Vehicle.loadVehicleDefinition(this)

  /**
    * Override this method to perform any special setup that is not standardized to `ToolDefinition`.
    * @see `Vehicle.loadVehicleDefinition`
    */
  protected def loadVehicleDefinition() : Unit = { }

  /**
    * Apply the roll value to all weapons whose controlling seats are unoccupied.
    * @param look a positive value clamped between n >= 0 degrees and n < 360 degrees
    */
  override def setRoll(look : Float) : Unit = {
    super.setRoll(look)
    for((_, seat) <- seats) {
      val wep : Option[Int] = seat.getControlledWeapon
      if(!seat.isOccupied && wep.isDefined) {
        weapons(wep.get).getEquipment.get.setRoll(look)
      }
    }
  }

  /**
    * Apply the pitch value to all weapons whose controlling seats are unoccupied.
    * @param look a value clamped between n > -90 degrees and n < 90 degrees
    */
  override def setPitch(look : Float) : Unit = {
    super.setPitch(look)
    for((_, seat) <- seats) {
      val occupied : Boolean = seat.isOccupied
      val wep : Option[Int] = seat.getControlledWeapon
      if(!occupied && wep.isDefined) {
        weapons(wep.get).getEquipment.get.setPitch(look)
      }
    }
  }

  /**
    * Apply the yaw value to all weapons whose controlling seats are unoccupied.
    * @param look a value clamped between n > -90 degrees and n < 90 degrees
    */
  override def setYaw(look : Float) : Unit = {
    super.setYaw(look)
    for((_, seat) <- seats) {
      val occupied : Boolean = seat.isOccupied
      val wep : Option[Int] = seat.getControlledWeapon
      if(!occupied && wep.isDefined) {
        weapons(wep.get).getEquipment.get.setYaw(look)
      }
    }
  }

  /**
    * This is the definition entry that is used to store and unload pertinent information about the `Vehicle`.
    * @return the vehicle's definition entry
    */
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

  def getMaxShields : Int = {
    this.vehicleDef.maxShields
  }

  def getDeployState : DriveState.Value = {
    this.deployed
  }

  def setDeployState(deploy : DriveState.Value) : DriveState.Value = {
    this.deployed = deploy
    deploy
  }

  def getDecal : Int = {
    this.decal
  }

  def setDecal(decal : Int) : Int = {
    this.decal = decal
    decal
  }

  /**
    * Given the index of an entry mounting point, return the infantry-accessible `Seat` associated with it.
    * @param mountPoint an index representing the seat position / mounting point
    * @return a seat number, or `None`
    */
  def getSeatFromMount(mountPoint : Int) : Option[Int] = {
    this.vehicleDef.mountPoints.get(mountPoint)
  }

  /**
    * Get the seat at the index.
    * The specified "seat" can only accommodate a player as opposed to weapon mounts which share the same indexing system.
    * @param seatNumber an index representing the seat position / mounting point
    * @return a `Seat`, or `None`
    */
  def getSeat(seatNumber : Int) : Option[Seat] = {
    if(seatNumber >= 0 && seatNumber < this.seats.size) {
      this.seats.get(seatNumber)
    }
    else {
      None
    }
  }

  /**
    * Get the weapon at the index.
    * @param wepNumber an index representing the seat position / mounting point
    * @return a weapon, or `None`
    */
  def getWeapon(wepNumber : Int) : Option[Equipment] = {
    val slot = this.weapons.get(wepNumber)
    if(slot.isDefined) {
      slot.get.getEquipment
    }
    else {
      None
    }
  }

  /**
    * Given a player who may be a passenger, retrieve an index where this player is seated.
    * @param player the player
    * @return a seat by index, or `None` if the `player` is not actually seated in this `Vehicle`
    */
  def passengerInSeat(player : PlayerAvatar) : Option[Int] = {
    var outSeat : Option[Int] = None
    val GUID = PlanetSideGUID(player.guid)
    for((seatNumber, seat) <- this.seats) {
      val occupant : Option[PlanetSideGUID] = seat.getOccupant
      if(occupant.isDefined && occupant.get == GUID) {
        outSeat = Some(seatNumber)
      }
    }
    outSeat
  }

  /**
    * Given a valid seat number, retrieve an index where a weapon controlled from this seat is attached.
    * @param seatNumber the seat number
    * @return a mounted weapon by index, or `None` if either the seat doesn't exist or there is no controlled weapon
    */
  def getWeaponControlledFromSeat(seatNumber : Int) : Option[Int] = {
    val seat : Option[Seat] = this.getSeat(seatNumber)
    var weapon : Option[Int] = None
    if(seat.isDefined) {
      weapon = seat.get.getControlledWeapon
    }
    weapon
  }

  /**
    * Get a refernece ot a certain `Utility` attached to this `Vehicle`.
    * @param utilNumber the attachment number of the `Utility`
    * @return the `Utility` or `None` (if invalid)
    */
  def getUtility(utilNumber : Int) : Option[Utility] = {
    if(utilNumber >= 0 && utilNumber < this.utilities.size) {
      Some(this.utilities(utilNumber))
    }
    else {
      None
    }
  }

  /**
    * A reference to the `Vehicle` `Trunk` space.
    * @return this `Vehicle` `Trunk`
    */
  def getTrunk : Trunk = {
    this.trunk
  }

  /**
    * Can this `player` access the contents of this `Vehicle`'s `Trunk` given its current access permissions?
    * @param player a player attempting to access this `Trunk`
    * @return `true`, if the `player` is permitted access; `false`, otherwise
    */
  def canUseTrunk(player : PlayerAvatar) : Boolean = {
    var access : Boolean = false
    trunkLockState match {
      case VehicleLockState.Locked => //only the owner
        access = owner.isEmpty || (owner.isDefined && PlanetSideGUID(player.guid) == owner.get)
      case VehicleLockState.Group => //anyone in the owner's squad or platoon
        access = faction == player.faction //TODO this is not correct
      case VehicleLockState.Empire => //anyone of the owner's faction
        access = faction == player.faction
    }
    access
  }

  /**
    * Check access to the `Trunk`.
    * @return the current access value for the `Vehicle` `Trunk`
    */
  def getTrunkLockState :  VehicleLockState.Value = {
    this.trunkLockState
  }

  /**
    * Change the access value for the trunk.
    * @param lockState the new access value for the `Vehicle` `Trunk`
    * @return the current access value for the `Vehicle` `Trunk` after the change
    */
  def setTrunkLockState(lockState :  VehicleLockState.Value) :  VehicleLockState.Value = {
    this.trunkLockState = lockState
    lockState
  }

  /**
    * Override the string representation to provide additional information.
    * @return the string output
    */
  override def toString : String = {
    Vehicle.toString(this)
  }
}

object Vehicle {
  /**
    * Overloaded constructor.
    * @param guid the vehicle's globally unique identifier
    * @param vehicleDef the vehicle's definition entry
    * @return a `Vwehicle` object
    */
  def apply(guid : Int, vehicleDef : VehicleDefinition) : Vehicle = {
    new Vehicle(guid, vehicleDef)
  }

  /**
    * Use the `VehicleDefinition` that was provided to this `Vehicle` to initialize its fields and settings.
    * @param vehicle the `Vehicle` being initialized
    * @see `loadVehicleDefinition`
    */
  def loadVehicleDefinition(vehicle : Vehicle) : Unit = {
    val vdef : VehicleDefinition = vehicle.getVehicleDefinition
    //general stuff
    vehicle.setHealth(vdef.maxHealth)
    //create weapons
    for((num, _/*definition*/) <- vdef.weapons) {
      val slot = EquipmentSlot(EquipmentSize.VEHICLEWEAPON)
      slot.setEquipment(new Tool(0, 0/*definition*/)) //TODO for "new Tool(0, 0)," the second param should be a ToolDefinition
      vehicle.weapons += num -> slot
    }
    //create seats
    for((num, seat) <- vdef.seats) {
      val newSeat = Seat(vehicle)
      newSeat.setBailable(seat.getBailable)
      newSeat.setArmorRestriction(seat.getArmorRestriction)
      newSeat.setControlledWeapon(seat.getControlledWeapon)
      vehicle.seats += num -> newSeat
    }
    //trunk
    vehicle.trunk = Trunk(vdef.trunkSize.width, vdef.trunkSize.height, vdef.trunkOffset)
    //TODO utilies must be loaded on a case-by-case basis
    vehicle.loadVehicleDefinition()
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(obj : Vehicle) : String = {
    s"{vehicle-${obj.getVehicleDefinition.name}-${obj.getOwner.toString}::(${obj.getHealth}/${obj.getMaxHealth})(-${obj.getShields}/${obj.getMaxShields})}"
  }
}
