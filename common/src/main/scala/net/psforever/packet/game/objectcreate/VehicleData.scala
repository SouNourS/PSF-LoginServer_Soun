// Copyright (c) 2017 PSForever
package net.psforever.packet.game.objectcreate

import net.psforever.packet.{Marshallable, PacketHelpers}
import scodec.codecs._
import scodec.{Attempt, Codec, Err}
import shapeless.{::, HNil}

/**
  * na
  * @param basic data common to objects
  * @param health the amount of health the object has, as a percentage of a filled bar
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

  def allTypesAllowed(item : Any) : Boolean = true

  def mountedUtilitiesCodec(typeCheck : (Any) => Boolean = allTypesAllowed) : Codec[List[InternalSlot]] = (
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
        for(item <- list) {
          if(!typeCheck(item.obj)) {
            Attempt.failure(Err(s"vehicle mount decodes into a disallowed type - $item"))
          }
        }
        Attempt.successful(list)

      case _ =>
        Attempt.failure(Err("invalid mounting data format"))
    },
    {
      case list =>
        if(list.size >= 255) {
          Attempt.failure(Err("vehicle encodes too many weapon mountings (255+ types!)"))
        }
        for(item <- list) {
          if(!typeCheck(item.obj)) {
            Attempt.failure(Err(s"vehicle mount is a disallowed type - $item"))
          }
        }
        Attempt.successful(list.size :: 0 :: list :: HNil)
    }
  )

  def codec(mount_capacity : Int = 1)(typeCheck : (Any) => Boolean = allTypesAllowed) : Codec[VehicleData] = (
    ("basic" | CommonFieldData.codec) ::
      uint2L ::
      ("health" | uint8L) ::
      uintL(7) ::
      ("unk" | uint4L) ::
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
        if(objMountCapacity > -1 && mount_capacity > -1) {
          if(mount_capacity != objMountCapacity) {
            Attempt.failure(Err(s"different encoding expectations for amount of mounts - actual $objMountCapacity, expected $mount_capacity"))
          }
          else if(mount_capacity != 0) {
            Attempt.failure(Err(s"vehicle encodes wrong number of mounts - actual 0, expected $mount_capacity"))
          }
        }
        Attempt.successful(basic :: 0 :: health :: 0 :: unk :: 0 :: None :: HNil)

      case obj @ VehicleData(basic, health, unk, mountings) =>
        val mountSize : Int = mountings.get.size
        val objMountCapacity = obj.mount_capacity
        if(objMountCapacity > -1 && mount_capacity > -1) {
          if(mount_capacity != objMountCapacity) {
            Attempt.failure(Err(s"different encoding expectations for amount of mounts - actual $objMountCapacity, expected $mount_capacity"))
          }
          else if(mount_capacity != mountSize) {
            Attempt.failure(Err(s"vehicle encodes wrong number of mounts - actual $mountSize, expected $mount_capacity"))
          }
        }
        Attempt.successful(basic :: 0 :: health :: 0 :: unk :: 0 :: mountings :: HNil)

      case _ =>
        Attempt.failure(Err("invalid vehicle data format"))
    }
  )

  implicit val codec : Codec[VehicleData] = codec()()
}
