// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.Codec
import scodec.codecs._

final case class QueueTimedHelpMessage(messages : List[String],
                                       unk : Boolean)
  extends PlanetSideGamePacket {
  type Packet = QueueTimedHelpMessage
  def opcode = GamePacketOpcode.QueueTimedHelpMessage
  def encode = QueueTimedHelpMessage.encode(this)
}

object QueueTimedHelpMessage extends Marshallable[QueueTimedHelpMessage] {
  implicit val codec : Codec[QueueTimedHelpMessage] = (
    ("messages" | listOfN(uint4L, PacketHelpers.encodedString)) ::
      ("unk" | bool)
    ).as[QueueTimedHelpMessage]
}
