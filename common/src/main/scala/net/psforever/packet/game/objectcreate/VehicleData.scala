// Copyright (c) 2017 PSForever
package net.psforever.packet.game.objectcreate

import net.psforever.packet.Marshallable
import scodec.codecs._
import scodec.{Attempt, Codec, Err}
import shapeless.{::, HNil}

/**
  * na
  * @param deploy data common to objects spawned by the (advanced) adaptive construction engine
  * @param health the amount of health the object has, as a percentage of a filled bar
  * @param internals data regarding the mounted weapon
  */
final case class VehicleData(deploy : ACEDeployableData,
                             health : Int,
                             internals : Option[List[InternalSlot]] = None
                            )(implicit val weapon_count : Int = 1) extends ConstructorData {
  override def bitsize : Long = {
    val deploySize = deploy.bitsize
    val internalSize = if(internals.isDefined) {
      var bSize : Long = 0L
      for(item <- internals.get) {
        bSize += item.bitsize
      }
      10 + bSize
    }
    else {
      0L
    }
    24L + deploySize + internalSize //2u + 8u + 7u + 4u + 2u + 1u
  }
}

object VehicleData extends Marshallable[VehicleData] {
  /**
    * Overloaded constructor that mandates information about the mounted weapons.
    * @param deploy data common to objects spawned by the (advanced) adaptive construction engine
    * @param health the amount of health the object has, as a percentage of a filled bar
    * @param internals data regarding the mounted weapon
    * @return a `SmallTurretData` object
    */
  def apply(deploy : ACEDeployableData,  health : Int,  internals : InternalSlot) : VehicleData =
    new VehicleData(deploy, health, Some(internals :: Nil))

  def codec(weapon_count : Int = 1) : Codec[VehicleData] = (
    ("deploy" | ACEDeployableData.codec) ::
      uint2L ::
      ("health" | uint8L) ::
      uintL(7) ::
      uint4L ::
      uint2L ::
      optional(bool, "internals" | InventoryData.inventoryCodec(InventoryItem.codec))
    ).exmap[VehicleData] (
    {
      case deploy :: 0 :: health :: 0 :: 0 :: 0 :: None ::HNil =>
        Attempt.successful(VehicleData(deploy, health, None))

      case deploy :: 0 :: health :: 0 :: 0 :: 0 :: internals :: HNil =>
        val list2 : List[InternalSlot] = internals.get.contents.flatMap(i => List[InternalSlot](i.item))
        val list : Option[List[InternalSlot]] = if(list2.nonEmpty) { Some(list2) } else { None }
        Attempt.successful(VehicleData(deploy, health, list))

      case _ =>
        Attempt.failure(Err("invalid small turret data format"))
    },
    {
      case VehicleData(deploy, health, None) =>
        Attempt.successful(deploy :: 0 :: health :: 0 :: 0 :: 0 :: None :: HNil)

      case VehicleData(deploy, health, list) =>
        val internals : List[InventoryItem] = list.get.flatMap(i => List(InventoryItem(i)))
        Attempt.successful(deploy :: 0 :: health :: 0 :: 0 :: 0 :: Some(InventoryData(internals)) :: HNil)
    }
  )

  implicit val codec : Codec[VehicleData] = codec()
}
