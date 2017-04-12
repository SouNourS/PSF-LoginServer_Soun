// Copyright (c) 2017 PSForever
package game

import org.specs2.mutable._
import net.psforever.packet._
import net.psforever.packet.game._
import net.psforever.types.Vector3
import scodec.bits._

class VehicleStateMessageTest extends Specification {
//  val string = hex"1b470a973077cb0ecc030fbaa813d1604475d6000078"
//  val string = hex"1be70d02dff43a80f8e33f5fe7504822958c5a09c003c0"
  val string = hex"1be70d06c0376486fa433f5fe7504822958c5a09c003c0"

//  "decode" in {
//    PacketCoding.DecodePacket(string).require match {
//      case VehicleStateMessage(guid,unk1,pos,vel,unk2,unk3,unk4,unk5,bool1,bool2) =>
//        guid mustEqual PlanetSideGUID(3559)
//        unk1 mustEqual 0
//        pos mustEqual Vector3(5630.172f,3712.2266f,103.109375f)
//        vel mustEqual Some(Vector3(255.65625f,-123.09375f,-189.75f))
//        unk2 mustEqual Some(9)
//        unk3 mustEqual 44
//        unk4 mustEqual 6
//        unk5 mustEqual 5
//        bool1 mustEqual true
//        bool2 mustEqual false
//      case _ =>
//        ko
//    }
//  }
  "decode" in {
    PacketCoding.DecodePacket(string).require match {
      case VehicleStateMessage(guid,unk1,pos,vel,unk2,unk3,unk4,unk5,bool1,bool2) =>
        guid mustEqual PlanetSideGUID(3559)
        unk1 mustEqual 0
        pos mustEqual Vector3(5634.422f,3719.3906f,103.28125f)
        vel mustEqual Some(Vector3(255.65625f,-123.09375f,-189.75f))
        unk2 mustEqual Some(9)
        unk3 mustEqual 44
        unk4 mustEqual 6
        unk5 mustEqual 5
        bool1 mustEqual true
        bool2 mustEqual false
      case _ =>
        ko
    }
  }

//  "encode" in {
////    val msg = VehicleStateMessage(PlanetSideGUID(2631), 4, Vector3(5895.4453f,3343.789f,97.5f), None, Some(30), 117, 5, 0, false, true)
////    val msg = VehicleStateMessage(PlanetSideGUID(3559), 0, Vector3(5630.172f,3712.2266f,103.109375f), Some(Vector3(255.65625f,-123.09375f,-189.75f)), Some(9), 44, 6, 5, true, false)
//    val msg = VehicleStateMessage(PlanetSideGUID(3559), 0, Vector3(5634.422f,3719.3906f,103.28125f), Some(Vector3(255.65625f,-123.09375f,-189.75f)), Some(9), 44, 6, 5, true, false)
//    val pkt = PacketCoding.EncodePacket(msg).require.toByteVector
//
//    pkt mustEqual string
//  }
}
