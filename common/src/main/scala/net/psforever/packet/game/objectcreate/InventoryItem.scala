// Copyright (c) 2017 PSForever
package net.psforever.packet.game.objectcreate

import net.psforever.packet.game.PlanetSideGUID
import scodec.Codec

object InventoryItem {
  /**
    * Alias `InventoryItem` to `InternalSlot`.
    */
  type InventoryItem = InternalSlot

  /**
    * Constructor for creating an `InventoryItem`.
    * @param guid the GUID this object will be assigned
    * @param slot a parent-defined slot identifier that explains where the child is to be attached to the parent
    * @param obj the data used as representation of the object to be constructed
    * @return a `InventoryItem` object
    */
  def apply(objClass : Int, guid : PlanetSideGUID, slot : Int, obj : ConstructorData) : InventoryItem =
    InternalSlot(objClass, guid, slot, obj)

  /**
    * A `Codec` for `0x17` `ObjectCreateMessage` data.
    */
  val codec : Codec[InventoryItem] = InternalSlot.codec

  /**
    * A `Codec` for `0x18` `ObjectCreateDetailedMessage` data.
    */
  val codec_detailed : Codec[InventoryItem] = InternalSlot.codec_detailed
}
