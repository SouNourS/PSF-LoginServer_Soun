// Copyright (c) 2017 PSForever
package game

import org.specs2.mutable._
import net.psforever.packet._
import net.psforever.packet.game._
import net.psforever.types.Vector3
import scodec.bits._

class HackMessageTest extends Specification {
  val string = hex"542a812684d900001c10818200000000"

  "decode" in {
    PacketCoding.DecodePacket(string).require match {
      case HackMessage(unk1,unk2,unk3,unk4,unk5,unk6,unk7) =>
        unk1 mustEqual 0
        unk2 mustEqual 1194
        unk3 mustEqual 5018
        unk4 mustEqual 100
        unk5 mustEqual 1114636288
        unk6 mustEqual 6
        unk7 mustEqual 8
      case _ =>
        ko
    }
  }

  "encode" in {
    val msg = HackMessage(0, 1194, 5018, 100, 1114636288, 6, 8)
    val pkt = PacketCoding.EncodePacket(msg).require.toByteVector

    pkt mustEqual string
  }
}
