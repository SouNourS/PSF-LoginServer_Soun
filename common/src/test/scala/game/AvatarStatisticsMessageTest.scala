// Copyright (c) 2017 PSForever
package game

import org.specs2.mutable._
import net.psforever.packet._
import net.psforever.packet.game.{AvatarStatisticsMessage, _}
import scodec.bits._

class AvatarStatisticsMessageTest extends Specification {
  val string_simple = hex"7F 4000000000"
  val string_complex = hex"7F 16E020400000012000000040000001C000000040000000200000000000000000000000"

  "decode (simple)" in {
    PacketCoding.DecodePacket(string_simple).require match {
      case AvatarStatisticsMessage(u1, u2, u3) =>
        u1 mustEqual 2
        u2.isDefined mustEqual true
        u2.get mustEqual 0L
        u3.isDefined mustEqual false
      case _ =>
        ko
    }
  }

  "decode (complex)" in {
    PacketCoding.DecodePacket(string_complex).require match {
      case AvatarStatisticsMessage(u1, u2, u3) =>
        u1 mustEqual 0
        u2.isDefined mustEqual false
        u3.isDefined mustEqual true
        u3.get.unk1 mustEqual 22
        u3.get.unk2 mustEqual 480
        u3.get.unk3.size mustEqual 4
        //0
        u3.get.unk3.head.unk1 mustEqual 2L
        u3.get.unk3.head.unk2 mustEqual 9L
        //1
        u3.get.unk3(1).unk1 mustEqual 2L
        u3.get.unk3(1).unk2 mustEqual 14L
        //2
        u3.get.unk3(2).unk1 mustEqual 2L
        u3.get.unk3(2).unk2 mustEqual 1L
        //3
        u3.get.unk3(3).unk1 mustEqual 0L
        u3.get.unk3(3).unk2 mustEqual 0L
      case _ =>
        ko
    }
  }

  "encode (simple)" in {
    val msg = AvatarStatisticsMessage(2, 0L)
    val pkt = PacketCoding.EncodePacket(msg).require.toByteVector

    pkt mustEqual string_simple
  }

  "encode (complex)" in {
    val msg = AvatarStatisticsMessage(
      0,
      22,
      480,
      AvatarStatisticsExtra2(2L, 9L) ::
        AvatarStatisticsExtra2(2L, 14L) ::
        AvatarStatisticsExtra2(2L, 1L) ::
        AvatarStatisticsExtra2(0L, 0L) ::
        Nil
    )
    val pkt = PacketCoding.EncodePacket(msg).require.toByteVector

    pkt mustEqual string_complex
  }

  "encode (failures)" in {
    //field format
    val msg1 = AvatarStatisticsMessage(0, None, None)
    PacketCoding.EncodePacket(msg1).isFailure mustEqual true

    //only three list entries
    val msg2 = AvatarStatisticsMessage(
      0,
      22,
      480,
      AvatarStatisticsExtra2(2L, 9L) ::
        AvatarStatisticsExtra2(2L, 14L) ::
        AvatarStatisticsExtra2(2L, 1L) ::
        Nil
    )
    PacketCoding.EncodePacket(msg2).isFailure mustEqual true
  }
}
