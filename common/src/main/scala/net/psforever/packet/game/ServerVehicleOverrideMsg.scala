// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/**
  * na
  * @param unk1 true = lock vehicle control
  * @param unk2 na
  * @param reverse true seem "reverse"
  * @param unk4 na
  * @param unk5 na
  * @param unk6 na
  * @param speed 'something like vehicle speed (1 for AMS is 3kph, 2 -> 7, 3 -> 10, 10 -> 35, 15 -> 52, 20 -> 68)'
  * @param unk8 na
  */
final case class ServerVehicleOverrideMsg(unk1 : Boolean,
                                          unk2 : Boolean,
                                          reverse : Boolean,
                                          unk4 : Boolean,
                                          unk5 : Int,
                                          unk6 : Int,
                                          speed : Int,
                                          unk8 : Option[Long] = None
                                         ) extends PlanetSideGamePacket {
  type Packet = ServerVehicleOverrideMsg
  def opcode = GamePacketOpcode.ServerVehicleOverrideMsg
  def encode = ServerVehicleOverrideMsg.encode(this)
}

object ServerVehicleOverrideMsg extends Marshallable[ServerVehicleOverrideMsg] {
  implicit val codec: Codec[ServerVehicleOverrideMsg] = (
    ("unk1" | bool) ::
      (("unk2" | bool) >>:~ { test =>
        ("reverse" | bool) ::
          ("unk4" | bool) ::
          ("unk5" | uint2L) ::
          ("unk6" | uint2L) ::
          ("speed" | uintL(9)) ::
          conditional(test, "unk8" | uint32L)
      })
    ).as[ServerVehicleOverrideMsg]
}
