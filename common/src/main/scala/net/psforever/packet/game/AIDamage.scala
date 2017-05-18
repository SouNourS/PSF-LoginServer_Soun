// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

final case class AIDamage(guid1 : PlanetSideGUID,
                          guid2 : PlanetSideGUID,
                          unk1 : Long,
                          unk2 : Long,
                          unk3 : Long)
  extends PlanetSideGamePacket {
  type Packet = AIDamage
  def opcode = GamePacketOpcode.AIDamage
  def encode = AIDamage.encode(this)
}

object AIDamage extends Marshallable[AIDamage] {
  implicit val codec : Codec[AIDamage] = (
    ("guid1" | PlanetSideGUID.codec) ::
      ("guid2" | PlanetSideGUID.codec) ::
      ("unk1" | uint32L) ::
      ("unk2" | uint32L) ::
      ("unk3" | uint32L)
    ).as[AIDamage]
}
