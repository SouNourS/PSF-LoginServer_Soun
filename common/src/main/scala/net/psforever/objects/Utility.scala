// Copyright (c) 2017 PSForever
package net.psforever.objects

import net.psforever.packet.game.PlanetSideEmpire

/**
  * A `Utility` represents an unknown but functional entity that is attached to a `Vehicle` and is not a weapon or a seat.
  * @param objectId the object id the is associated with this sort of `Utility`
  * @param vehicle the `Vehicle` to which this `Utility` is attached
  */
class Utility(val objectId : Int, vehicle : Vehicle) {
  private var active : Boolean = false

  /**
    * The faction association of this `Utility` is tied directly to the connected `Vehicle`.
    * @return the faction association
    */
  def getFaction : PlanetSideEmpire.Value = {
    vehicle.getFaction
  }

  /**
    * An "active" `Utility` can be used by players; an "inactive" one can not be used in its current state.
    * @return whether this `Utility` is active.
    */
  def getActiveState : Boolean = {
    this.active
  }

  /**
    * Change the "active" state of this `Utility`.
    * @param state the new active state
    * @return the current active state after being changed
    */
  def setActiveState(state : Boolean) : Boolean = {
    this.active = state
    state
  }

  /**
    * Override the string representation to provide additional information.
    * @return the string output
    */
  override def toString : String = {
    Utility.toString(this)
  }
}

object Utility {
  /**
    * An overloaded constructor.
    * @param objectId the object id the is associated with this sort of `Utility`
    * @param vehicle the `Vehicle` to which this `Utility` is attached
    * @return a `Utility` object
    */
  def apply(objectId : Int, vehicle : Vehicle) : Utility = {
    new Utility(objectId, vehicle)
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(obj : Utility) : String = {
    s"{utility-${obj.objectId}}"
  }
}
