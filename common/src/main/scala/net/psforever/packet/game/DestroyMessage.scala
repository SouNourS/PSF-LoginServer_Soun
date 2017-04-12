// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import net.psforever.types.Vector3
import scodec.Codec
import scodec.codecs._

/**
  * na
  * @param victim_GUID the victim's GUID
  * @param killer_GUID the killer's GUID
  * @param killer_weapon_GUID the GUID of killer's weapon
  * @param pos last victim position
  */
final case class DestroyMessage(victim_GUID : Int,
                                killer_GUID : Int,
                                killer_weapon_GUID : Int,
                                pos : Vector3)
  extends PlanetSideGamePacket {
  type Packet = DestroyMessage
  def opcode = GamePacketOpcode.DestroyMessage
  def encode = DestroyMessage.encode(this)
}

object DestroyMessage extends Marshallable[DestroyMessage] {
  implicit val codec : Codec[DestroyMessage] = (
    ("victim_GUID" | uint16L) ::
      ("killer_GUID" | uint16L) ::
      ("killer_weapon_GUID" | uint16L) ::
      ("pos" | Vector3.codec_pos)
    ).as[DestroyMessage]
}
