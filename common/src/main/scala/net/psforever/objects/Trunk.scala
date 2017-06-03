// Copyright (c) 2017 PSForever
package net.psforever.objects

/**
  * The `Trunk` that belongs to some `Vehicle`.
  * This represents the storage space where ammunition and other `Tools` are stored.<br>
  * <br>
  * This inheritance of normal inventory is necessary to allow the indexOffset to be manually configured.
  * If the trunk does not keep track of the first official cell space index in the grid-like inventory, objects will be misplaced.
  * @param width the width of the inventory
  * @param height the height of the inventory
  * @param offset the first cell offset value
  */
class Trunk(width : Int, height : Int, offset : Int = 0) extends ListInventory(width, height) {
  override val indexOffset = offset

  /**
    * Override the string representation to provide additional information.
    * @return the string output
    */
  override def toString : String = {
    Trunk.toString(this)
  }
}

object Trunk {
  /**
    * Overloaded constructor.
    * @param width the width of the inventory
    * @param height the height of the inventory
    * @param offset the first cell offset value
    * @return a `Trunk` object
    */
  def apply(width : Int, height : Int, offset : Int) : Trunk = {
    new Trunk(width, height, offset)
  }

  /**
    * Provide a fixed string representation.
    * @return the string output
    */
  def toString(obj : Trunk) : String = {
    "{trunk(%dx%d): %d items}".format(obj.width, obj.height, obj.size)
  }
}
