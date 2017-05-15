// Copyright (c) 2017 PSForever
package net.psforever.packet.game.objectcreate

import net.psforever.packet.{Marshallable, PacketHelpers}
import net.psforever.packet.game.PlanetSideGUID
import scodec.codecs._
import scodec.{Attempt, Codec, Err}
import shapeless.{::, HNil}

/**
  * An `Enumeration` of the deployment states of the Advanced Mobile Station.
  */
object AMSDeployState extends Enumeration {
  type Type = Value

  val Mobile, //drivable
      Undeployed, //stationary, ejects possible game object intersections
      DeployedButUnavailable, //stationary, spawn cloak bubble active, utilities are inaccessible
      Deployed //stationary, spawn cloak bubble active, utilities functional, spawn-ready
      = Value

  implicit val codec = PacketHelpers.createEnumerationCodec(this, uint2L)
}

/**
  * na
  * @param basic data common to objects
  * @param unk1 na
  * @param health the amount of health the object has, as a percentage of a filled bar
  * @param deployState the spawn-readiness condition
  * @param unk2 na;
  *             common values are 0 or 63;
  *             usually in a non-`Mobile` state when non-zero
  * @param matrix_term the spawn matrix terminal on the front
  * @param respawn_tube the respawn terminal on the rear
  * @param order_term_a the equipment terminal on the left side
  * @param order_term_b the equipment terminal on the right side
  */
final case class AMSData(basic : CommonFieldData,
                         unk1 : Int,
                         health : Int,
                         deployState : AMSDeployState.Value,
                         unk2 : Int,
                         matrix_term : InternalSlot,
                         respawn_tube : InternalSlot,
                         order_term_a : InternalSlot,
                         order_term_b : InternalSlot
                        ) extends ConstructorData {
  override def bitsize : Long = {
    val basicSize = basic.bitsize
    val vehicleSize : Long = VehicleData.baseVehicleDataSize
    val utilitySize : Long = 4 * matrix_term.bitsize //the four utilities should all be the same size
    16L + basicSize + vehicleSize + utilitySize
  }
}

object AMSData extends Marshallable[AMSData] {
  /**
    * Overloaded constructor for a standard Advanced Mobile Station vehicle that resolves its utilities, providing GUIDs.
    * @param basic data common to objects
    * @param unk1 na
    * @param health the amount of health the object has, as a percentage of a filled bar
    * @param deployState the spawn-readiness condition
    * @param unk2 na
    * @param matrix_guid the GUID for the spawn matrix panel
    * @param respawn_guid the GUID for the respawn apparatus
    * @param terma_guid the GUID for a terminal on the AMS
    * @param termb_guid the GUID for a terminal on the AMS
    * @return
    */
  def apply(basic : CommonFieldData, unk1 : Int, health : Int, deployState : AMSDeployState.Value, unk2 : Int, matrix_guid : PlanetSideGUID, respawn_guid : PlanetSideGUID, terma_guid : PlanetSideGUID, termb_guid : PlanetSideGUID) : AMSData = {
    AMSData(basic, unk1, health, deployState, unk2,
      InternalSlot(ObjectClass.matrix_terminalc, matrix_guid, 1, CommonTerminalData(basic.faction)),
      InternalSlot(ObjectClass.ams_respawn_tube, respawn_guid,2, CommonTerminalData(basic.faction)),
      InternalSlot(ObjectClass.order_terminala,  terma_guid,  3, CommonTerminalData(basic.faction)),
      InternalSlot(ObjectClass.order_terminalb,  termb_guid,  4, CommonTerminalData(basic.faction))
    )
  }

  implicit val codec : Codec[AMSData] = (
    ("basic" | CommonFieldData.codec) :: //note: am not certain if player_guid is a valid field for AMS context
      ("unk1" | uint2L) :: //note: may be tied to previous questionable field boundary
      ("health" | uint8L) ::
      uint8L ::
      ("deployState" | AMSDeployState.codec) ::
      bool ::
      ("unk2" | uintL(6)) ::
      bool ::
      uintL(12) ::
      ("term1" | InternalSlot.codec) ::
      ("tube" | InternalSlot.codec) ::
      ("term2" | InternalSlot.codec) ::
      ("term3" | InternalSlot.codec)
    ).exmap[AMSData] (
    {
      case basic :: unk1 :: health :: 0 :: deployState :: false :: unk2 :: false :: 0x41 ::
        InternalSlot(ObjectClass.matrix_terminalc, matrix_guid, 1, com_term1) ::
        InternalSlot(ObjectClass.ams_respawn_tube, respawn_guid,2, com_term2) ::
        InternalSlot(ObjectClass.order_terminala,  terma_guid,  3, com_term3) ::
        InternalSlot(ObjectClass.order_terminalb,  termb_guid,  4, com_term4) :: HNil =>
        Attempt.successful(
          AMSData(basic, unk1, health, deployState, unk2,
            InternalSlot(ObjectClass.matrix_terminalc, matrix_guid, 1, com_term1),
            InternalSlot(ObjectClass.ams_respawn_tube, respawn_guid,2, com_term2),
            InternalSlot(ObjectClass.order_terminala,  terma_guid,  3, com_term3),
            InternalSlot(ObjectClass.order_terminalb,  termb_guid,  4, com_term4)
          )
        )

      case _ =>
        Attempt.failure(Err("invalid AMS data"))
    },
    {
      case AMSData(basic, unk1, health, deployState, unk2,
            InternalSlot(ObjectClass.matrix_terminalc, matrix_guid, 1, com_term1),
            InternalSlot(ObjectClass.ams_respawn_tube, respawn_guid,2, com_term2),
            InternalSlot(ObjectClass.order_terminala,  terma_guid,  3, com_term3),
            InternalSlot(ObjectClass.order_terminalb,  termb_guid,  4, com_term4)) =>
        Attempt.successful(
          basic :: unk1 :: health :: 0 :: deployState :: false :: unk2 :: false :: 0x41 ::
            InternalSlot(ObjectClass.matrix_terminalc, matrix_guid, 1, com_term1) ::
            InternalSlot(ObjectClass.ams_respawn_tube, respawn_guid,2, com_term2) ::
            InternalSlot(ObjectClass.order_terminala,  terma_guid,  3, com_term3) ::
            InternalSlot(ObjectClass.order_terminalb,  termb_guid,  4, com_term4) :: HNil
        )

      case _ =>
        Attempt.failure(Err("invalid AMS data"))
    }
  )
}
