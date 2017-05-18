// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

final case class OffshoreVehicleMessage(guid1 : PlanetSideGUID,
                                        guid2 : PlanetSideGUID,
                                        unk : Int)
  extends PlanetSideGamePacket {
  type Packet = OffshoreVehicleMessage
  def opcode = GamePacketOpcode.OffshoreVehicleMessage
  def encode = OffshoreVehicleMessage.encode(this)
}

object OffshoreVehicleMessage extends Marshallable[OffshoreVehicleMessage] {
  implicit val codec : Codec[OffshoreVehicleMessage] = (
    ("guid1" | PlanetSideGUID.codec) ::
      ("guid2" | PlanetSideGUID.codec) ::
      ("unk" | uint2L)
    ).as[OffshoreVehicleMessage]
}
