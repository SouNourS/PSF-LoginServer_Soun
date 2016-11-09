// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

final case class PlanetsideAttributeMessage(item_guid : PlanetSideGUID,
                                            unk1 : Long,
                                            beret  : Boolean,
                                            earpiece : Boolean,
                                            sunglasses : Boolean,
                                            hat : Boolean,
                                            helmet : Boolean)
  extends PlanetSideGamePacket {
  type Packet = PlanetsideAttributeMessage
  def opcode = GamePacketOpcode.PlanetsideAttributeMessage
  def encode = PlanetsideAttributeMessage.encode(this)
}

object PlanetsideAttributeMessage extends Marshallable[PlanetsideAttributeMessage] {
  implicit val codec : Codec[PlanetsideAttributeMessage] = (
    ("player_guid" | PlanetSideGUID.codec) ::
      ("unk1" | uint32L) ::
      ("beret" | bool) ::
      ("earpiece" | bool) ::
      ("sunglasses" | bool) ::
      ("hat" | bool) ::
      ("helmet" | bool)
    ).as[PlanetsideAttributeMessage]
}
