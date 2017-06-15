// Copyright (c) 2017 PSForever
import net.psforever.objects._
import org.specs2.mutable._

class VehicleObjectTest extends Specification {
  "Vehicle" should {
    "constructor" in {
      val vehicle : Vehicle = Vehicle(205, VehicleDefinition.Fury)
      vehicle.getMaxHealth mustEqual vehicle.getMaxHealth
    }
  }
}
