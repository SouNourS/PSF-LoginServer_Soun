// Copyright (c) 2017 PSForever
package net.psforever.packet.game

import net.psforever.packet.{GamePacketOpcode, Marshallable, PacketHelpers, PlanetSideGamePacket}
import scodec.{Attempt, Codec, Err}
import scodec.codecs._
import shapeless.{::, HNil}

final case class AvatarStatisticsExtra1(unk1 : Int,
                                        unk2 : Int,
                                        unk3 : List[AvatarStatisticsExtra2])

final case class AvatarStatisticsExtra2(unk1 : Long,
                                        unk2 : Long)

final case class AvatarStatisticsMessage(unk1 : Int,
                                         unk2 : Option[Long],
                                         unk3 : Option[AvatarStatisticsExtra1] = None
                                        ) extends PlanetSideGamePacket {
  type Packet = AvatarStatisticsMessage
  def opcode = GamePacketOpcode.AvatarStatisticsMessage
  def encode = AvatarStatisticsMessage.encode(this)
}

object AvatarStatisticsMessage extends Marshallable[AvatarStatisticsMessage] {
  def apply(unk1 : Int, unk2 : Long) : AvatarStatisticsMessage = {
    AvatarStatisticsMessage(unk1, Some(unk2))
  }

  def apply(unk1 : Int, unk2 : Int, unk3 : Int, unk4 : List[AvatarStatisticsExtra2]) : AvatarStatisticsMessage = {
    AvatarStatisticsMessage(unk1, None, Some(AvatarStatisticsExtra1(unk2, unk3, unk4)))
  }

  private val extra2_codec : Codec[AvatarStatisticsExtra2] = (
    ("unk1" | uint32L) ::
      ("unk2" | uint32L)
  ).as[AvatarStatisticsExtra2]

  private val extra1_codec : Codec[AvatarStatisticsExtra1] = (
    ("unk1" | uintL(5)) ::
      ("unk2" | uintL(11)) ::
      ("unk3" | PacketHelpers.listOfNSized(4, extra2_codec))
    ).exmap[AvatarStatisticsExtra1] (
    {
      case u1 :: u2 :: u3 :: HNil =>
        Attempt.successful(AvatarStatisticsExtra1(u1, u2, u3))
    },
    {
      case AvatarStatisticsExtra1(u1, u2, u3) =>
        if(u3.size < 4) {
          Attempt.failure(Err("not enough list items - need 4"))
        }
        else {
          Attempt.successful(u1 :: u2 :: u3.take(4) :: HNil)
        }
    }
  )

  implicit val codec : Codec[AvatarStatisticsMessage] = (
    ("unk1" | uintL(3)) >>:~ { unk1 =>
      conditional(unk1 - 2 == 1 || unk1 - 2 == 0, "unk2" | uint32L) ::
      conditional(unk1 - 2 > 1 || unk1 - 2 < 0, "unk3" | extra1_codec)
    }).exmap[AvatarStatisticsMessage] (
    {
      case u1 :: u2 :: u3 :: HNil =>
        Attempt.successful(AvatarStatisticsMessage(u1, u2, u3))
    },
    {
      case AvatarStatisticsMessage(2, Some(u2), _) =>
        Attempt.successful(2 :: Some(u2) :: None :: HNil)

      case AvatarStatisticsMessage(3, Some(u2), _) =>
        Attempt.successful(3 :: Some(u2) :: None :: HNil)

      case AvatarStatisticsMessage(n, _, Some(u3)) =>
        Attempt.successful(n :: None :: Some(u3) :: HNil)

      case AvatarStatisticsMessage(n, _, _) =>
        Attempt.failure(Err(s"bad switch value - $n does not allow for this combination of input"))
    }
  )
}
