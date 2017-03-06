// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

/**
  * Represent a piece of Equipment placed into the spatial grid of an inventory.
  * @param obj the equipment being stowed
  * @param y the row of the upper left corner of the equipment's tile
  * @param x the column of the upper left corner of the equipment's tile
  */
class InventoryItem2(val obj : Equipment, val y : Int, val x : Int) {
  // if the object does not exist, do not create
  if(Option(obj).isEmpty)
    throw new IllegalArgumentException("can not build an inventory container without an item to be contained")

  /**
    * Get the inventory size of the object being represented.
    * @see Equipment.getInventorySize
    * @return a Tuple containing (1) the height of the tile and (2) the width of the tile
    */
  def getInventorySize : (Int, Int) = {
    obj.getInventorySize
  }

  /**
    * Override the string representation to provide additional information.
    * @return the string output
    */
  override def toString : String = {
    InventoryItem2.toString(this)
  }
}

object InventoryItem2 {
  /**
    * A constructor that accepts the minimum parameters.
    * @param obj the Equipment being stowed
    * @param y the row of the upper left corner of the equipment's tile
    * @param x the column of the upper left corner of the equipment's tile
    * @return the InventoryItem2
    */
  def apply(obj : Equipment, y : Int, x : Int) = {
    new InventoryItem2(obj, y, x)
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(obj : InventoryItem2) : String = {
    "<%s>".format(obj.obj.toString)
  }
}
