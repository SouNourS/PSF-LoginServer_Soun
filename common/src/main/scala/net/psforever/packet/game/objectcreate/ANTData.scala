// Copyright (c) 2017 PSForever
package net.psforever.packet.game.objectcreate

import net.psforever.packet.{Marshallable, PacketHelpers}
import scodec.codecs._
import scodec.{Attempt, Codec, Err}
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
  * @param unk1 na
  * @param health the amount of health the object has, as a percentage of a filled bar
  * @param unk2 na
  * @param deployState the nanite interaction-readiness condition;
  *                    defaults to `Mobile`
  * @param unk3 na;
  *             defaults to 0
  */
final case class ANTData(basic : CommonFieldData,
                         unk1 : Int,
                         health : Int,
                         unk2 : Int,
                         deployState : ANTDeployState.Value = ANTDeployState.Mobile,
                         unk3 : Int = 0
                        ) extends ConstructorData {
  override def bitsize : Long = {
    val basicSize = basic.bitsize
    val vehicleBasicSize : Long = VehicleData.baseVehicleSize
    9L + basicSize + vehicleBasicSize
  }
}

object ANTData extends Marshallable[ANTData] {
  /**
    * Overloaded constructor.
    * @param basic data common to objects
    * @param health the amount of health the object has, as a percentage of a filled bar
    * @param deployState the nanite interaction-readiness condition
    * @return an `ANTData` object
    */
  def apply(basic : CommonFieldData, health : Int, deployState : ANTDeployState.Value) : ANTData =
    new ANTData(basic, 0, health, 0, deployState, 0)

  implicit val codec : Codec[ANTData] = (
    VehicleData.basic_vehicle_codec.exmap[VehicleData.basicVehiclePattern] (
      {
        case basic :: u1 :: health :: u2 :: u3 :: u4 :: HNil =>
          Attempt.successful(basic :: u1 :: health :: u2 :: u3 :: u4 :: HNil)
      },
      {
        case basic :: u1 :: health :: u2 :: u3 :: u4 :: HNil =>
          Attempt.successful(basic :: u1 :: health :: u2 :: u3 :: u4 :: HNil)
      }
    ) :+
      uint8L :+
      bool //false for vehicle driving control; ditto u4 from above
    ).exmap[ANTData] (
    {
      case basic :: unk1 :: health :: unk2 :: deployState :: false :: unk3 :: false :: HNil =>
        Attempt.successful(ANTData(basic, unk1, health, unk2, ANTDeployState(deployState), unk3))

      case _ =>
        Attempt.failure(Err("invalid ant data format"))
    },
    {
      case ANTData(basic, unk1, health, unk2, deployState, unk3) =>
        Attempt.successful(basic :: unk1 :: health :: unk2 :: deployState.id :: false :: unk3 :: false :: HNil)
    }
  )
}
