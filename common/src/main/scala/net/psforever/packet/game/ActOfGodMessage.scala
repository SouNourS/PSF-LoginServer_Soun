// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import net.psforever.types.Vector3
import scodec.Codec
import scodec.codecs._

/**
  * maybe 100% not good :D
  */
final case class ActOfGodMessage(unk1 : Int,
                                 pos : Vector3,
                                 unk2 : Int,
                                 unk3 : Long,
                                 unk4 : Long)
  extends PlanetSideGamePacket {
  type Packet = ActOfGodMessage
  def opcode = GamePacketOpcode.ActOfGodMessage
  def encode = ActOfGodMessage.encode(this)
}

object ActOfGodMessage extends Marshallable[ActOfGodMessage] {
  implicit val codec : Codec[ActOfGodMessage] = (
    ("unk1" | uint2L) ::
      ("pos" | Vector3.codec_pos) ::
      ("unk2" | uint16L) ::
      ("unk3" | uint32L) ::
      ("unk4" | uint32L)
    ).as[ActOfGodMessage]
}
