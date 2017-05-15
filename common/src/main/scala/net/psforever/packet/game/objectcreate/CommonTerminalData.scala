// Copyright (c) 2017 PSForever
package net.psforever.packet.game.objectcreate

import net.psforever.packet.Marshallable
import net.psforever.types.PlanetSideEmpire
import scodec.{Attempt, Codec, Err}
import scodec.codecs._
import shapeless.{::, HNil}

/**
  * A representation of an object that can be interacted with when using a variety of terminals.
  * This object is generally invisible.
  * @param faction the faction that can access the terminal
  * @param unk na
  */
final case class CommonTerminalData(faction : PlanetSideEmpire.Value,
                                    unk : Int = 0
                                   ) extends ConstructorData {
  override def bitsize : Long = 24L
}

object CommonTerminalData extends Marshallable[CommonTerminalData] {
  implicit val codec : Codec[CommonTerminalData] = (
    ("faction" | PlanetSideEmpire.codec) ::
      uint2L ::
      ("unk" | uint2L) ::
      uint(18)
    ).exmap[CommonTerminalData] (
    {
      case fac :: 0 :: unk :: 0 :: HNil =>
        Attempt.successful(CommonTerminalData(fac, unk))
      case _ :: _ :: _ :: _ :: HNil =>
        Attempt.failure(Err("invalid terminal data format"))
    },
    {
      case CommonTerminalData(fac, unk) =>
        Attempt.successful(fac :: 0 :: unk :: 0 :: HNil)
    }
  )
}
