// Copyright (c) 2017 PSForever
package net.psforever.packet.game.objectcreate

import net.psforever.packet.{Marshallable, PacketHelpers}
import net.psforever.packet.game.PlanetSideGUID
import scodec.{Attempt, Codec, Err}
import scodec.codecs._
import shapeless.{::, HNil}

/**
  * A representation of a class of weapons that can be created using `ObjectCreateDetailedMessage` packet data.
  * This data will help construct a "loaded weapon" such as a Suppressor or a Gauss.<br>
  * <br>
  * The data for the weapons nests information for the default (current) type and number of ammunition in its magazine.
  * This ammunition data essentially is the weapon's magazines as numbered slots.
  * An "expected" number of ammunition slot data can be passed into the function
  * @param unk1 na
  * @param unk2 na
  * @param ammo data regarding the currently loaded ammunition type(s) and quantity(ies)
  * @param mag_size implicit;
  *                 the total number of concurrently-loaded ammunition types allowed in this weapon;
  *                 concurrent ammunition does not need to be unloaded to be switched;
  *                 defaults to 1;
  *                 0 or less ignores the imposed check
  * @see `DetailedAmmoBoxData`
  */
final case class DetailedWeaponData(unk1 : Int,
                                    unk2 : Int,
                                    ammo : List[InternalSlot]
                                   )(implicit val mag_size : Int = 1) extends ConstructorData {
  override def bitsize : Long = {
    var bitsize : Long = 0L
    for(o <- ammo) {
      bitsize += o.bitsize
    }
    61L + bitsize
  }
}

object DetailedWeaponData extends Marshallable[DetailedWeaponData] {
  /**
    * Overloaded constructor for creating `DetailedWeaponData` while masking use of `InternalSlot` for its `DetailedAmmoBoxData`.
    * @param unk1 na
    * @param unk2 na
    * @param cls the code for the type of object (ammunition) being constructed
    * @param guid the globally unique id assigned to the ammunition
    * @param parentSlot the slot where the ammunition is to be installed in the weapon
    * @param ammo the constructor data for the ammunition
    * @return a `DetailedWeaponData` object
    */
  def apply(unk1 : Int, unk2 : Int, cls : Int, guid : PlanetSideGUID, parentSlot : Int, ammo : DetailedAmmoBoxData) : DetailedWeaponData =
    new DetailedWeaponData(unk1, unk2, InternalSlot(cls, guid, parentSlot, ammo) :: Nil)

  /**
    * A `Codec` for `DetailedWeaponData`
    * @param magSize the total number of concurrently-loaded ammunition types allowed in this weapon;
    *                 defaults to 1
    * @return a `WeaponData` object or a `BitVector`
    */
  def codec(magSize : Int = 1) : Codec[DetailedWeaponData] = (
    ("unk1" | uint4L) ::
      ("unk2" | uint4L) ::
      uint24 ::
      uint16 ::
      uint2L ::
      (uint8L >>:~ { size =>
        uint2L ::
          ("ammo" | PacketHelpers.listOfNSized(size, InternalSlot.codec_detailed)) ::
          bool
      })
    ).exmap[DetailedWeaponData] (
    {
      case unk1 :: unk2 :: 2 :: 0 :: 3 :: size :: 0 :: ammo :: false :: HNil =>
        if(size != ammo.size)
          Attempt.failure(Err(s"weapon decodes wrong number of ammunition - actual ${ammo.size}, expected $size"))
        else if(magSize >= 1 && size != magSize)
          Attempt.failure(Err(s"weapon decodes too much or too little ammunition - actual $size, expected $magSize"))
        else
          Attempt.successful(DetailedWeaponData(unk1, unk2, ammo)(magSize))

      case _ =>
        Attempt.failure(Err("invalid weapon data format"))
    },
    {
      case obj @ DetailedWeaponData(unk1, unk2, ammo) =>
        val objMagSize = obj.mag_size
        val size = ammo.size
        if(objMagSize >= 1 && size != objMagSize)
          Attempt.failure(Err(s"weapon encodes too much or too little ammunition - actual $size, expected $objMagSize"))
        else if(size >= 255)
          Attempt.failure(Err("weapon encodes too much ammunition (255+ types!)"))
        else
            Attempt.successful(unk1 :: unk2 :: 2 :: 0 :: 3 :: size :: 0 :: ammo :: false :: HNil)
    }
  )

  implicit val codec : Codec[DetailedWeaponData] = codec()
}
