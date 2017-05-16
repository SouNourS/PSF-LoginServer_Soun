// Copyright (c) 2017 PSForever
package net.psforever.packet.game.objectcreate

import net.psforever.packet.{Marshallable, PacketHelpers}
import scodec.codecs._
import scodec.{Attempt, Codec, Err}
import shapeless.{::, HNil}

/**
  * A representation of a generic vehicle, with optional mounted weapons.
  * This data will help construct most of the game's vehicular options such as the Lightning or the Harasser.<br>
  * <br>
  * Vehicles utilize their own packet to communicate position to the server, known as `VehicleStateMessage`.
  * This takes the place of `PlayerStateMessageUpstream` when the player avatar is in control;
  * and, it takes the place of `PlayerStateMessage` for other players when they are in control.
  * If the vehicle is sufficiently complicated, a `ChildObjectStateMessage` will be used.
  * This packet will control any turret(s) on the vehicle.
  * For very complicated vehicles, the packets `FrameVehicleStateMessage` and `VehicleSubStateMessage` will also be employed.
  * The tasks that these packets perform are different based on the vehicle that responds or generates them.<br>
  * <br>
  * Much like an inventory (though not actually the vehicle's inventory), weapons are placed into "slots" in this vehicle.
  * In general, these slots are numbered outside of the vehicle's normal Infantry seating count.
  * The acceptable weapon mounting slots, and the Infantry seat that asserts control of the weapon, are defined elsewhere.
  * Such an alternate relationship can not be expressed in a packet of this data.
  * An "expected" number of mounting data can be passed into the class for the purposes of validating input.<br>
  * <br>
  * Outside of managing mounted weaponry, any vehicle with special "utilities" must be handled as a special case.
  * This includes vehicles that go through a sessile physical conversion known as "deploying."
  * @param basic data common to objects
  * @param health the amount of health the vehicle has, as a percentage of a filled bar
  * @param mountings data regarding the mounted utilities, usually weapons
  * @param mount_capacity implicit;
  *                       the total number of mounted utilities allowed on this vehicle;
  *                       defaults to 1;
  *                       -1 or less ignores the imposed checks
  */
final case class VehicleData(basic : CommonFieldData,
                             health : Int,
                             unk : Int,
                             mountings : Option[List[InternalSlot]] = None
                            )(implicit val mount_capacity : Int = 1) extends ConstructorData {
  override def bitsize : Long = {
    val basicSize = basic.bitsize
    val internalSize = if(mountings.isDefined) {
      var bSize : Long = 0L
      for(item <- mountings.get) {
        bSize += item.bitsize
      }
      10 + bSize
    }
    else {
      0L
    }
    VehicleData.baseVehicleDataSize + basicSize + internalSize
  }
}

object VehicleData extends Marshallable[VehicleData] {
  val baseVehicleDataSize : Long = 24L //2u + 8u + 7u + 4u + 2u + 1u

  /**
    * Overloaded constructor that mandates information about a single weapon.
    * @param basic data common to objects
    * @param health the amount of health the object has, as a percentage of a filled bar
    * @param mount data regarding the mounted weapon
    * @return a `VehicleData` object
    */
  def apply(basic : CommonFieldData, health : Int, mount : InternalSlot) : VehicleData =
    new VehicleData(basic, health, 0, Some(mount :: Nil))

  /**
    * Perform an evaluation of the provided object.
    * @param list a List of objects to be compared against some criteria
    * @return `true`, if the objects pass this test; false, otherwise
    */
  def allAllowed(list : List[InternalSlot]) : Boolean = true

  /**
    * A `Codec` for mounted utilities, generally weapons (as `WeaponData`).
    * @param slotChecker a function that takes an `InternalSlot` and returns `true` if that object passed its defined test;
    *                    defaults to `allAllowed`
    * @return a `VehicleData` object or a `BitVector`
    */
  def mountedUtilitiesCodec(slotChecker : (List[InternalSlot]) => Boolean = allAllowed) : Codec[List[InternalSlot]] = (
    uint8L >>:~ { size =>
      uint2L ::
        PacketHelpers.listOfNSized(size, InternalSlot.codec)
    }).exmap[List[InternalSlot]] (
    {
      case _ :: 0 :: list :: HNil =>
        val listSize = list.size
        if(listSize >= 255) {
          Attempt.failure(Err("vehicle decodes too many mountings (255+ types!)"))
        }
        else if(!slotChecker(list)) {
          Attempt.failure(Err("vehicle mount decoding is disallowed by test failure"))
        }
        else {
          Attempt.successful(list)
        }

      case _ =>
        Attempt.failure(Err("invalid mounting data format"))
    },
    {
      case list =>
        if(list.size >= 255) {
          Attempt.failure(Err("vehicle encodes too many weapon mountings (255+ types!)"))
        }
        else if(!slotChecker(list)) {
          Attempt.failure(Err("vehicle mount encoding is disallowed by test failure"))
        }
        else {
          Attempt.successful(list.size :: 0 :: list :: HNil)
        }
    }
  )

  /**
    * A `Codec` for `VehicleData`.
    * @param mount_capacity the total number of mounted weapons that are attached to this vehicle;
    *                       defaults to 1
    * @param typeCheck a function that takes an object and returns `true` if the object passed its defined test;
    *                  defaults to `allAllowed`
    * @return a `VehicleData` object or a `BitVector`
    */
  def codec(mount_capacity : Int = 1)(typeCheck : (List[InternalSlot]) => Boolean = allAllowed) : Codec[VehicleData] = (
    ("basic" | CommonFieldData.codec) ::
      uint2L ::
      ("health" | uint8L) ::
      uintL(7) ::
      ("unk" | uint4L) :: //1 is jammered?
      uint2L ::
      optional(bool, "mountings" | mountedUtilitiesCodec(typeCheck))
    ).exmap[VehicleData] (
    {
      case basic :: 0 :: health :: 0 :: unk :: 0 :: None :: HNil =>
        if(mount_capacity > -1 && mount_capacity != 0) {
          Attempt.failure(Err(s"vehicle decodes wrong number of mounts - actual 0, expected $mount_capacity"))
        }
        else {
          Attempt.successful(VehicleData(basic, health, unk, None)(0))
        }

      case basic :: 0 :: health :: 0 :: unk :: 0 :: mountings :: HNil =>
        val onboardMountCount : Int = mountings.get.size
        if(mount_capacity > -1 && mount_capacity != onboardMountCount) {
          Attempt.failure(Err(s"vehicle decodes wrong number of mounts - actual $onboardMountCount, expected $mount_capacity"))
        }
        else {
          Attempt.successful(VehicleData(basic, health, unk, mountings)(onboardMountCount))
        }

      case _ =>
        Attempt.failure(Err("invalid vehicle data format"))
    },
    {
      case obj @ VehicleData(basic, health, unk, None) =>
        val objMountCapacity = obj.mount_capacity
        if(objMountCapacity < 0 || mount_capacity < 0) {
          Attempt.successful(basic :: 0 :: health :: 0 :: unk :: 0 :: None :: HNil)
        }
        else {
          if(mount_capacity != objMountCapacity) {
            Attempt.failure(Err(s"different encoding expectations for amount of mounts - actual $objMountCapacity, expected $mount_capacity"))
          }
          else if(mount_capacity != 0) {
            Attempt.failure(Err(s"vehicle encodes wrong number of mounts - actual 0, expected $mount_capacity"))
          }
          else {
            Attempt.successful(basic :: 0 :: health :: 0 :: unk :: 0 :: None :: HNil)
          }
        }

      case obj @ VehicleData(basic, health, unk, mountings) =>
        val objMountCapacity = obj.mount_capacity
        if(objMountCapacity < 0 || mount_capacity < 0) {
          Attempt.successful(basic :: 0 :: health :: 0 :: unk :: 0 :: mountings :: HNil)
        }
        else {
          val mountSize : Int = mountings.get.size
          if(mount_capacity != objMountCapacity) {
            Attempt.failure(Err(s"different encoding expectations for amount of mounts - actual $objMountCapacity, expected $mount_capacity"))
          }
          else if(mount_capacity != mountSize) {
            Attempt.failure(Err(s"vehicle encodes wrong number of mounts - actual $mountSize, expected $mount_capacity"))
          }
          else {
            Attempt.successful(basic :: 0 :: health :: 0 :: unk :: 0 :: mountings :: HNil)
          }
        }

      case _ =>
        Attempt.failure(Err("invalid vehicle data format"))
    }
  )

  implicit val codec : Codec[VehicleData] = codec()()
}
