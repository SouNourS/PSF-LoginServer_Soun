// Copyright (c) 2017 PSForever
package game

import org.specs2.mutable._
import net.psforever.packet._
import net.psforever.packet.game._
import net.psforever.types.Vector3
import scodec.bits._

class ActOfGodMessageTest extends Specification {
  val string = hex"ce400000000000000100000028100c1d400000"

  "decode" in {
    PacketCoding.DecodePacket(string).require match {
      case ActOfGodMessage(unk1,pos,unk2,unk3,unk4) =>
        unk1 mustEqual 1
        pos mustEqual Vector3(0.0f,0.0f,0.0f)
        unk2 mustEqual 4
        unk3 mustEqual 1084227584
        unk4 mustEqual 30000
      case _ =>
        ko
    }
  }

  "encode" in {
    val msg = ActOfGodMessage(1, Vector3(0f,0f,0f), 4, 1084227584,  30000)
    val pkt = PacketCoding.EncodePacket(msg).require.toByteVector

    pkt mustEqual string
  }
}
