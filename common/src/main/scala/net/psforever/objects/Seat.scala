// Copyright (c) 2017 PSForever
package net.psforever.objects

import net.psforever.packet.game.PlanetSideGUID
import net.psforever.types.PlanetSideEmpire

/**
  * An `Enumeration` of exo-suit-based seat access restrictions.<br>
  * <br>
  * The default value is `NoMax` as that is the most common seat.
  * `NoReinforcedOrMax` is next most common.
  * `MaxOnly` is a rare seat restriction found in pairs on Galaxies and on the large "Ground Transport" vehicles.
  */
object SeatArmorRestriction extends Enumeration {
  type Type = Value

  val MaxOnly,
      NoMax,
      NoReinforcedOrMax
      = Value
}

/**
  * Server-side support for a slot that infantry players can occupy, ostensibly called a "seat" and treated like a "seat."
  * @param vehicle the vehicle where this seat is installed
  */
class Seat(private val vehicle : Option[Vehicle]) {
  private var occupant : Option[PlanetSideGUID] = None
  private var armorRestriction : SeatArmorRestriction.Value = SeatArmorRestriction.NoMax
  private var lockState :  VehicleLockState.Value =  VehicleLockState.Empire
  private var bailable : Boolean = false
  private var weaponSeat : Option[Int] = None

  /**
    * The faction association of this `Seat` is tied directly to the connected `Vehicle`.
    * @return the faction association, or `NEUTRAL` if there is no defined `Vehicle`
    */
  def getFaction : PlanetSideEmpire.Value = {
    if(vehicle.isDefined) { vehicle.get.getFaction } else { PlanetSideEmpire.NEUTRAL }
  }

  /**
    * Is this seat occupied?
    * @return the GUID of the player sitting in this seat, or `None` if it is left vacant
    */
  def getOccupant : Option[PlanetSideGUID] = {
    this.occupant
  }

  /**
    * The player is trying to sit down.
    * Seats are exclusive positions that can only hold one occupant at a time.
    * @param player the player who wants to sit, or `None` if the occupant is getting up
    * @return the GUID of the player sitting in this seat, or `None` if it is left vacant
    */
  def setOccupant(player : Option[PlayerAvatar]) : Option[PlanetSideGUID] = {
    if(this.occupant.isEmpty) {
      this.occupant = Some(PlanetSideGUID(player.get.guid))
    }
    else if(player.isEmpty) {
      this.occupant = None
    }
    this.occupant
  }

  /**
    * Is this seat occupied?
    * @return `true`, if it is occupied; `false`, otherwise
    */
  def isOccupied : Boolean = {
    this.occupant.isDefined
  }

  def getArmorRestriction : SeatArmorRestriction.Value = {
    this.armorRestriction
  }

  def setArmorRestriction(restriction : SeatArmorRestriction.Value) : SeatArmorRestriction.Value = {
    this.armorRestriction = restriction
    restriction
  }

  def getSeatLockState :  VehicleLockState.Value = {
    this.lockState
  }

  def setSeatLockState(lockState :  VehicleLockState.Value) :  VehicleLockState.Value = {
    this.lockState = lockState
    lockState
  }

  def getBailable : Boolean = {
    this.bailable
  }

  def setBailable(canBail : Boolean) : Boolean = {
    this.bailable = canBail
    canBail
  }

  def getControlledWeapon : Option[Int] = {
    this.weaponSeat
  }

  def setControlledWeapon(seat : Option[Int]) : Option[Int] = {
    this.weaponSeat = seat
    this.weaponSeat
  }

  /**
    * Given a player, can they access this `Seat` under its current restrictions and permissions.
    * @param player the player who wants to sit
    * @return `true` if the player can sit down in this `Seat`; `false`, otherwise
    */
  def canUseSeat(player : PlayerAvatar) : Boolean = {
    var access : Boolean = false
    val vehicle : Option[Vehicle] = this.vehicle
    val owner : Option[PlanetSideGUID] = if(vehicle.isDefined) { vehicle.get.getOwner } else { None }
    lockState match {
      case VehicleLockState.Locked =>
        access = owner.isEmpty || (owner.isDefined && PlanetSideGUID(player.guid) == owner.get)
      case VehicleLockState.Group => //TODO this is not correct
        access = getFaction == player.faction
      case VehicleLockState.Empire =>
        access = getFaction == player.faction
    }
    access
  }

  /**
    * Override the string representation to provide additional information.
    * @return the string output
    */
  override def toString : String = {
    Seat.toString(this)
  }
}

object Seat {
  /**
    * Overloaded constructor that allows for the existence of a seat by itself.
    * Useful when working with `VehicleDefinition`.
    * @return a `Seat` object
    */
  def apply() : Seat = {
    new Seat(None)
  }

  /**
    * Overloaded constructor.
    * @param vehicle the vehicle where this seat is installed
    * @return a `Seat` object
    */
  def apply(vehicle : Vehicle) : Seat = {
    new Seat(Some(vehicle))
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(obj : Seat) : String = {
    val weaponStr = if(obj.getControlledWeapon.isDefined) { " (gunner)" } else { "" }
    val seatStr = if(obj.isOccupied) {
      "occupied by %d".format(obj.getOccupant.get.guid)
    }
    else {
      "unoccupied"
    }
    s"{Seat$weaponStr: $seatStr}"
  }
}
