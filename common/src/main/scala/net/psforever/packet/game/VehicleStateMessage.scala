// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import net.psforever.types.Vector3
import scodec.Codec
import scodec.codecs._

// TODO not fully works

/**
  * partial
  * @param unk1 na
  */
final case class VehicleStateMessage(guid : PlanetSideGUID,
                                     unk1 : Int,
                                     pos : Vector3,
                                     vel : Option[Vector3],
                                     unk2 : Option[Int],
                                     unk3 : Int,
                                     unk4 : Int,
                                     unk5 : Int,
                                     bool1 : Boolean,
                                     bool2 : Boolean)
  extends PlanetSideGamePacket {
  type Packet = VehicleStateMessage
  def opcode = GamePacketOpcode.VehicleStateMessage
  def encode = VehicleStateMessage.encode(this)
}

object VehicleStateMessage extends Marshallable[VehicleStateMessage] {
  implicit val codec : Codec[VehicleStateMessage] = (
    ("guid" | PlanetSideGUID.codec) ::
      ("unk1" | uintL(3)) ::
      ("pos" | Vector3.codec_pos) ::
      ("vel" | optional(bool, Vector3.codec_vel)) ::
      ("unk2" | optional(bool, uintL(5))) ::
      ("unk3" | uintL(7)) ::
      ("unk4" | uintL(4)) ::
      ("unk5" | uintL(5)) ::
      ("bool1" | bool) ::
      ("bool2" | bool)
    ).as[VehicleStateMessage]
}
