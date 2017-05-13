// Copyright (c) 2017 PSForever
package net.psforever.packet.game.objectcreate

import net.psforever.packet.Marshallable
import scodec.codecs._
import scodec.Codec
import shapeless.{::, HNil}

/**
  * A representation of an Advanced nanite Transport vehicle that can be created using `ObjectCreateMessage` packet data.
  * ANTs accept no utility mountings.
  * @param basic data common to objects
  * @param health the amount of health the object has, as a percentage of a filled bar
  */
final case class ANTData(basic : CommonFieldData,
                         health : Int
                        ) extends ConstructorData {
  override def bitsize : Long = {
    val basicSize = basic.bitsize
    val vehicleBasicSize : Long = VehicleData.baseVehicleDataSize
    6L + vehicleBasicSize + basicSize
  }
}

object ANTData extends Marshallable[ANTData] {
  implicit val codec : Codec[ANTData] = (
    VehicleData.codec(0)() ::
      uintL(6)
    ).xmap[ANTData] (
    {
      case VehicleData(basic, health, _) :: 0 :: HNil =>
        ANTData(basic, health)
    },
    {
      case ANTData(basic, health) =>
        VehicleData(basic, health, None)(0) :: 0 :: HNil
    }
  )
}
