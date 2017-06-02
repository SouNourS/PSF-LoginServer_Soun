// Copyright (c) 2017 PSForever
package net.psforever.objects

import net.psforever.packet.game.{PlanetSideEmpire, PlanetSideGUID}

object SeatArmorRestriction extends Enumeration {
  type Type = Value

  val None,
      MaxOnly,
      NoReinforced,
      NoMax,
      NoReinforcedOrMax
      = Value
}

object SeatLockState extends Enumeration {
  type Type = Value

  val Empire,
      Squad,
      Locked
      = Value
}

class Seat() extends Equipment(0) {
  private var faction : PlanetSideEmpire.Value = PlanetSideEmpire.TR
  private var occupant : Option[PlanetSideGUID] = None
  private var armorRestriction : SeatArmorRestriction.Value = SeatArmorRestriction.None
  private var lockState : SeatLockState.Value = SeatLockState.Empire
  private var bailable : Boolean = false
  private var weaponSeat : Option[Int] = None

  def getFaction : PlanetSideEmpire.Value = {
    this.faction
  }

  def setFaction(faction : PlanetSideEmpire.Value) : PlanetSideEmpire.Value = {
    this.faction = faction
    faction
  }

  def getOccupant : Option[PlanetSideGUID] = {
    this.occupant
  }

  def setOccupant(player : Option[PlayerAvatar]) : Option[PlanetSideGUID] = {
    if(this.occupant.isEmpty) {
      this.occupant = Some(PlanetSideGUID(player.get.guid))
    }
    else if(player.isEmpty) {
      this.occupant = None
    }
    this.occupant
  }

  def getArmorRestriction : SeatArmorRestriction.Value = {
    this.armorRestriction
  }

  def setArmorRestriction(restriction : SeatArmorRestriction.Value) : SeatArmorRestriction.Value = {
    this.armorRestriction = restriction
    restriction
  }

  def getSeatLockState : SeatLockState.Value = {
    this.lockState
  }

  def setSeatLockState(lockState : SeatLockState.Value) : SeatLockState.Value = {
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
}

object Seat {
  def apply() : Seat = {
    new Seat()
  }
}
