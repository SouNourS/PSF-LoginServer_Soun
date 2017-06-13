// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._


/**
  * Is sent by the server when the client has performed an action from a menu item
  * (i.e create character, delete character, etc...)
  * For a "CharacterCreateRequestMessage", error code 1 = already exist, 2 = not allowed
  */
final case class ActionResultMessage(successfull : Boolean, errorCode : Option[Long])
  extends PlanetSideGamePacket {
  type Packet = ActionResultMessage
  def opcode = GamePacketOpcode.ActionResultMessage
  def encode = ActionResultMessage.encode(this)
}

object ActionResultMessage extends Marshallable[ActionResultMessage] {
  implicit val codec : Codec[ActionResultMessage] = (
    ("successful" | bool) >>:~ { res =>
      // if not successful, look for an error code
      conditional(!res, "error_code" | uint32L).hlist
    }
    ).as[ActionResultMessage]
}