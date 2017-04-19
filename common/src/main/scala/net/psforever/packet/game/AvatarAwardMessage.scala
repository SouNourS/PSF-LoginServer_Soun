// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import net.psforever.types.Vector3
import scodec.Codec
import scodec.codecs._


final case class AvatarAwardMessage(u1 : Long,
                                    u2 : Option[Boolean])

  extends PlanetSideGamePacket {
  type Packet = AvatarAwardMessage
  def opcode = GamePacketOpcode.AvatarAwardMessage
  def encode = AvatarAwardMessage.encode(this)
}


object AvatarAwardMessage extends Marshallable[AvatarAwardMessage] {
  implicit val codec : Codec[AvatarAwardMessage] = (
    ("u1" | uint32L) ::
      optional(bool, "u2" | bool)
    ).as[AvatarAwardMessage]
}
