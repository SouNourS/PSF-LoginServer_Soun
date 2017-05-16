// Copyright (c) 2017 PSForever
package net.psforever.packet.game.objectcreate

import net.psforever.packet.{Marshallable, PacketHelpers}
import scodec.codecs._
import scodec.Codec
import shapeless.{::, HNil}

/**
  * An `Enumeration` of the deployment states of the Advanced Mobile Station.
  */
object ANTDeployState extends Enumeration {
  type Type = Value

  val Mobile, //drivable
      Undeployed, //stationary
      Deployed //stationary, flaps extended
      = Value

  implicit val codec = PacketHelpers.createEnumerationCodec(this, uint8L)
}

/**
  * A representation of a vehicle called the Advanced Nanite Transport (ANT).
  * @param basic data common to objects
  * @param health the amount of health the object has, as a percentage of a filled bar
  * @param deployState the nanite interaction-readiness condition;
  *                    defaults to `Mobile`
  */
final case class ANTData(basic : CommonFieldData,
                         health : Int,
                         deployState : ANTDeployState.Value = ANTDeployState.Mobile
                        ) extends ConstructorData {
  override def bitsize : Long = {
    val basicSize = basic.bitsize
    val vehicleBasicSize : Long = VehicleData.baseVehicleDataSize
    6L + vehicleBasicSize + basicSize
  }
}

object ANTData extends Marshallable[ANTData] {
  implicit val codec : Codec[ANTData] = (
    ("basic" | CommonFieldData.codec) ::
      uint2L ::
      ("health" | uint8L) ::
      uint2L :: //setting second bit stops displaying entry points
      ("deployState" | ANTDeployState.codec) ::
      bool :: //false, or can not control vehicle
      uint8L ::
      bool //false, or can not control vehicle
    ).xmap[ANTData] (
    {
      case basic :: 0 :: health :: 0 :: deployState :: false :: 0 :: false :: HNil =>
        ANTData(basic, health, deployState)
    },
    {
      case ANTData(basic, health, deployState) =>
        basic :: 0 :: health :: 0 :: deployState :: false :: 0 :: false :: HNil
    }
  )
}
