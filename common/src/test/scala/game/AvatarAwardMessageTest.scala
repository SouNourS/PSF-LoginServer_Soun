// Copyright (c) 2017 PSForever
package game

import org.specs2.mutable._
import net.psforever.packet._
import net.psforever.packet.game._
import net.psforever.types.Vector3
import scodec.bits._

class AvatarAwardMessageTest extends Specification {
//  val string = hex"cf5c00000000c00000240040000000"
  val string = hex"cf1501000006c000003d0040000000"

  "decode" in {
    PacketCoding.DecodePacket(string).require match {
      case AvatarAwardMessage(u1, u2) =>
//        u1 mustEqual 92
//        u2 mustEqual None
        u1 mustEqual 277
        u2 mustEqual None
      case _ =>
        ko
    }
  }

  "encode" in {
//    val msg = AvatarAwardMessage(92, None)
    val msg = AvatarAwardMessage(277, None)
    val pkt = PacketCoding.EncodePacket(msg).require.toByteVector

    pkt mustEqual string
  }
}
