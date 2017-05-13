// Copyright (c) 2017 PSForever
package net.psforever.packet.game.objectcreate

import net.psforever.packet.{Marshallable, PacketHelpers}
import net.psforever.packet.game.PlanetSideGUID
import scodec.{Attempt, Codec, Err}
import scodec.codecs._
import shapeless.{::, HNil}

/**
  * A representation of a class of weapons that can be created using `ObjectCreateMessage` packet data.
  * This data will help construct a "loaded weapon" such as a Suppressor or a Gauss.
  * Common uses include items deposited on the ground and items in another player's visible inventory (holsters).<br>
  * <br>
  * The data for the weapons nests information for the default (current) type and number of ammunition in its magazine.
  * This ammunition data essentially is the weapon's magazines as numbered slots.
  * An "expected" number of ammunition slot data can be passed into the function.
  * @param unk1 na;
  *             commonly 8
  * @param unk2 na;
  *             commonly 12
  * @param fire_mode the current mode of weapon's fire;
  *                  zero-indexed
  * @param ammo data regarding the currently loaded ammunition type(s)
  * @param mag_capacity implicit;
  *                     the total number of concurrently-loaded ammunition types allowed in this weapon;
  *                     concurrent ammunition does not need to be unloaded to be switched;
  *                     defaults to 1;
  *                     0 is invalid;
  *                     -1 or less ignores the imposed check
  * @see `AmmoBoxData`
  */
final case class WeaponData(unk1 : Int,
                            unk2 : Int,
                            fire_mode : Int,
                            ammo : List[InternalSlot]
                           )(implicit val mag_capacity : Int = 1) extends ConstructorData {
  override def bitsize : Long = {
    var bitsize : Long = 0L
    for(o <- ammo) {
      bitsize += o.bitsize
    }
    44L + bitsize
  }
}

object WeaponData extends Marshallable[WeaponData] {
  /**
    * Overloaded constructor for creating `WeaponData` while masking use of `InternalSlot` for its `AmmoBoxData`.
    * @param unk1 na
    * @param unk2 na
    * @param cls the code for the type of object (ammunition) being constructed
    * @param guid the globally unique id assigned to the ammunition
    * @param parentSlot the slot where the ammunition is to be installed in the weapon
    * @param ammo the ammunition object
    * @return a `WeaponData` object
    */
  def apply(unk1 : Int, unk2 : Int, cls : Int, guid : PlanetSideGUID, parentSlot : Int, ammo : AmmoBoxData) : WeaponData =
    new WeaponData(unk1, unk2, 0, InternalSlot(cls, guid, parentSlot, ammo) :: Nil)

  /**
    * Overloaded constructor for creating `WeaponData` while masking use of `InternalSlot` for its `AmmoBoxData`.
    * @param unk1 na
    * @param unk2 na
    * @param fire_mode data regarding the currently loaded ammunition type
    * @param cls the code for the type of object (ammunition) being constructed
    * @param guid the globally unique id assigned to the ammunition
    * @param parentSlot the slot where the ammunition is to be installed in the weapon
    * @param ammo the ammunition object
    * @return a `WeaponData` object
    */
  def apply(unk1 : Int, unk2 : Int, fire_mode : Int, cls : Int, guid : PlanetSideGUID, parentSlot : Int, ammo : AmmoBoxData) : WeaponData =
    new WeaponData(unk1, unk2, fire_mode, InternalSlot(cls, guid, parentSlot, ammo) :: Nil)

  /**
    * A `Codec` for `WeaponData`
    * @param mag_capacity the total number of concurrently-loaded ammunition types allowed in this weapon;
    *                     defaults to 1
    * @return a `WeaponData` object or a `BitVector`
    */
  def codec(mag_capacity : Int = 1) : Codec[WeaponData] = (
    ("unk1" | uint4L) ::
      ("unk2" | uint4L) ::
      uint(20) ::
      ("fire_mode" | int(3)) ::
      bool ::
      bool ::
      (uint8L >>:~ { size =>
        uint2L ::
          ("ammo" | PacketHelpers.listOfNSized(size, InternalSlot.codec)) ::
          bool
      })
    ).exmap[WeaponData] (
    {
      case unk1 :: unk2 :: 0 :: fmode :: false :: true :: size :: 0 :: ammo :: false :: HNil =>
        val magSize = ammo.size
        if(mag_capacity == 0 || magSize == 0) {
          Attempt.failure(Err("weapon must decode some ammunition"))
        }
        else if(size != magSize) {
          Attempt.failure(Err(s"weapon decodes wrong amount of ammunition - actual $magSize, expected $size"))
        }
        else if(mag_capacity > 0 && size != mag_capacity) {
          Attempt.failure(Err(s"weapon decodes too much or too little ammunition - actual $size, expected $mag_capacity"))
        }
        else {
          Attempt.successful(WeaponData(unk1, unk2, fmode, ammo)(magSize))
        }

      case _ =>
        Attempt.failure(Err("invalid weapon data format"))
    },
    {
      case obj @ WeaponData(unk1, unk2, fmode, ammo) =>
        val magSize = ammo.size
        val magCapacity = obj.mag_capacity
        if(mag_capacity == 0 || magCapacity == 0 || magSize == 0) {
          Attempt.failure(Err("weapon must encode some ammunition"))
        }
        else if(magCapacity != mag_capacity) {
          Attempt.failure(Err(s"different encoding expectations for amount of ammunition - actual $magCapacity, expected $mag_capacity"))
        }
        else if(mag_capacity > 0 && magSize != mag_capacity) {
          Attempt.failure(Err(s"weapon encodes wrong amount of ammunition - actual $magSize, expected $mag_capacity"))
        }
        else if(magSize >= 255) {
          Attempt.failure(Err("weapon encodes too much ammunition (255+ types!)"))
        }
        else {
          Attempt.successful(unk1 :: unk2 :: 0 :: fmode :: false :: true :: magSize :: 0 :: ammo :: false :: HNil)
        }

      case _ =>
        Attempt.failure(Err("invalid weapon data format"))
    }
  )

  implicit val codec : Codec[WeaponData] = codec()
}
