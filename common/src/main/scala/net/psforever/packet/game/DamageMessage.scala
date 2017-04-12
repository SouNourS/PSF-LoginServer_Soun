// Copyright (c) 2016 PSForever.net to present
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

/**
  * na
  * @param player_guid the player (victim ?)
  * @param unk2 na
  * @param weapon_guid na
  * @param unk4 na
  */
final case class DamageMessage(player_guid : PlanetSideGUID,
                               unk2 : Int,
                               weapon_guid : Int, //probably also a PlanetSideGUID, for another player?
                               unk4 : Boolean)
  extends PlanetSideGamePacket {
  type Packet = DamageMessage
  def opcode = GamePacketOpcode.DamageMessage
  def encode = DamageMessage.encode(this)
}

object DamageMessage extends Marshallable[DamageMessage] {
  implicit val codec : Codec[DamageMessage] = (
    ("player_guid" | PlanetSideGUID.codec) ::
      ("unk2" | uint8L) ::
      ("weapon_guid" | uint16L) ::
      ("unk4" | bool)
    ).as[DamageMessage]
}
