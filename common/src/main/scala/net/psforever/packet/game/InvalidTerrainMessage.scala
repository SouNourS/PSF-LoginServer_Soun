// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import net.psforever.types.Vector3
import scodec.Codec
import scodec.codecs._

/**
  * na
  * @param unk1 na
  * @param unk2 na
  * @param unk3 na
  * @param pos na
  */
final case class InvalidTerrainMessage(unk1 : Int,
                                       unk2 : Int,
                                       unk3 : Boolean,
                                       pos : Vector3)
  extends PlanetSideGamePacket {
  type Packet = InvalidTerrainMessage
  def opcode = GamePacketOpcode.InvalidTerrainMessage
  def encode = InvalidTerrainMessage.encode(this)
}

object InvalidTerrainMessage extends Marshallable[InvalidTerrainMessage] {
  implicit val codec : Codec[InvalidTerrainMessage] = (
    ("unk1" | uint16L) ::
      ("unk2" | uint16L) ::
      ("unk3" | bool) ::
      ("pos" | Vector3.codec_float)
    ).as[InvalidTerrainMessage]
}
