// Copyright (c) 2017 PSForever
package net.psforever.objects.serverobject.terminals

import akka.actor.Actor
import net.psforever.objects.serverobject.affinity.{FactionAffinity, FactionAffinityBehavior}
import net.psforever.packet.game.PlanetSideGUID

/**
  * An `Actor` that handles messages being dispatched to a specific `Terminal`.
  * @param term the `Terminal` object being governed
  */
class TerminalControl(term : Terminal) extends Actor with FactionAffinityBehavior.Check {
  def FactionObject : FactionAffinity = term

  def receive : Receive = checkBehavior.orElse {
    case Terminal.Request(player, msg) =>
      if ((term.GUID == PlanetSideGUID(3067) || term.GUID == PlanetSideGUID(283)) && term.Faction != player.Faction) {
        term.HackedBy = player // TODO remove that later !!!!!!!!!
      }
      sender ! Terminal.TerminalMessage(player, msg, term.Request(player, msg))

    case _ => ;
  }

  override def toString : String = term.Definition.Name
}
