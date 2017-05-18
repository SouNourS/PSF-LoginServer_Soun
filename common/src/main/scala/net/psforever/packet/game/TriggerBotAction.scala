// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

final case class TriggerBotAction(bot_guid : PlanetSideGUID,
                                  unk1 : Long = 0,
                                  unk2 : Long = 0,
                                  unk3 : Long = 0xFFFFFFFFL)
  extends PlanetSideGamePacket {
  type Packet = TriggerBotAction
  def opcode = GamePacketOpcode.TriggerBotAction
  def encode = TriggerBotAction.encode(this)
}

object TriggerBotAction extends Marshallable[TriggerBotAction] {
  implicit val codec : Codec[TriggerBotAction] = (
    ("bot_guid" | PlanetSideGUID.codec) ::
      ("unk1" | uint32L) ::
      ("unk2" | uint32L) ::
      ("unk3" | uint32L)
    ).as[TriggerBotAction]
}
