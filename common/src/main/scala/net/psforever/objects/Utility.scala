// Copyright (c) 2017 PSForever
package net.psforever.objects

import net.psforever.packet.game.PlanetSideEmpire

class Utility(val objectId : Int) {
  private var faction : PlanetSideEmpire.Value = PlanetSideEmpire.TR
  private var active : Boolean = false

  def getFaction : PlanetSideEmpire.Value = {
    this.faction
  }

  def setFaction(faction : PlanetSideEmpire.Value) : PlanetSideEmpire.Value = {
    this.faction = faction
    faction
  }

  def getActiveState : Boolean = {
    this.active
  }

  def setActiveState(state : Boolean) : Boolean = {
    this.active = state
    state
  }
}
