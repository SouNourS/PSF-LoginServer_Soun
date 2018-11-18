// Copyright (c) 2017 PSForever
import akka.actor.ActorContext
import net.psforever.objects.serverobject.pad.VehicleSpawnPad
import net.psforever.objects.serverobject.pad.process._
import net.psforever.objects.serverobject.resourcesilo.ResourceSilo
import net.psforever.objects.zones.Zone
import net.psforever.types.PlanetSideEmpire

object Zones {
  val z1 = new Zone("z1", Maps.map1, 1)

  val z2 = new Zone("z2", Maps.map2, 2){
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      import net.psforever.types.PlanetSideEmpire
      Building(10).get.Faction = PlanetSideEmpire.TR //Chac
      Building(10).get.ModelId = 45
    }
  }

  val z3 = new Zone("z3", Maps.map3, 3)//{
  //    override def Init(implicit context : ActorContext) : Unit = {
  //      super.Init(context)
  //
  //      import net.psforever.types.PlanetSideEmpire
  //      Building(5).get.Faction = PlanetSideEmpire.NC //Ekera
  //      Building(5).get.ModelId = 7
  //    }
  //  }

  val z4 = new Zone("z4", Maps.map4, 4) {
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)


      import net.psforever.types.PlanetSideEmpire

      Building(5).get.Faction = PlanetSideEmpire.TR //Akkan
      Building(6).get.Faction = PlanetSideEmpire.TR //Baal
      Building(7).get.Faction = PlanetSideEmpire.TR //Dagon
      Building(8).get.Faction = PlanetSideEmpire.NC //enkidu
      Building(9).get.Faction = PlanetSideEmpire.VS //Girru
      Building(10).get.Faction = PlanetSideEmpire.VS //Hanish
      Building(11).get.Faction = PlanetSideEmpire.VS //Irkalla
      Building(12).get.Faction = PlanetSideEmpire.VS //Kusag
      Building(13).get.Faction = PlanetSideEmpire.VS //Lahar
      Building(14).get.Faction = PlanetSideEmpire.NC //Marduk
      Building(15).get.Faction = PlanetSideEmpire.NC //Neti
      Building(16).get.Faction = PlanetSideEmpire.NC //Zaqar
      //      Building(17).get.Faction = PlanetSideEmpire.NC //S_Marduk_Tower
      //      Building(18).get.Faction = PlanetSideEmpire.NC //W_Neti_Tower
      //      Building(19).get.Faction = PlanetSideEmpire.NC //W_Zaqar_Tower
      //      Building(20).get.Faction = PlanetSideEmpire.NC //E_Zaqar_Tower
      //      Building(21).get.Faction = PlanetSideEmpire.NC //NE_Neti_Tower
      //      Building(22).get.Faction = PlanetSideEmpire.NC //SE_Ceryshen_Warpgate_Tower
      //      Building(23).get.Faction = PlanetSideEmpire.VS //S_Kusag_Tower
      //      Building(24).get.Faction = PlanetSideEmpire.VS //NW_Kusag_Tower
      //      Building(25).get.Faction = PlanetSideEmpire.VS //N_Ceryshen_Warpgate_Tower
      //      Building(26).get.Faction = PlanetSideEmpire.VS //SE_Irkalla_Tower
      //      Building(27).get.Faction = PlanetSideEmpire.VS //S_Irkalla_Tower
      Building(28).get.Faction = PlanetSideEmpire.TR //NE_Enkidu_Tower
      Building(29).get.Faction = PlanetSideEmpire.NC //SE_Akkan_Tower
      Building(30).get.Faction = PlanetSideEmpire.NC //SW_Enkidu_Tower
      //      Building(31).get.Faction = PlanetSideEmpire.TR //E_Searhus_Warpgate_Tower
      //      Building(32).get.Faction = PlanetSideEmpire.TR //N_Searhus_Warpgate_Tower
      //      Building(33).get.Faction = PlanetSideEmpire.VS //E_Girru_Tower
      //      Building(34).get.Faction = PlanetSideEmpire.VS //SE_Hanish_Tower
      Building(35).get.Faction = PlanetSideEmpire.TR //SW_Hanish_Tower
      //      Building(36).get.Faction = PlanetSideEmpire.VS //W_Girru_Tower
      //      Building(37).get.Faction = PlanetSideEmpire.TR //E_Dagon_Tower
      //      Building(38).get.Faction = PlanetSideEmpire.TR //NE_Baal_Tower
      //      Building(39).get.Faction = PlanetSideEmpire.TR //SE_Baal_Tower
      //      Building(40).get.Faction = PlanetSideEmpire.TR //S_Dagon_Tower
      //      Building(41).get.Faction = PlanetSideEmpire.NC //W_Ceryshen_Warpgate_Tower
      ////      Building(42).get.Faction = PlanetSideEmpire.NEUTRAL //dagon bunker
      ////      Building(43).get.Faction = PlanetSideEmpire.NEUTRAL //Akkan North Bunker
      ////      Building(44).get.Faction = PlanetSideEmpire.NEUTRAL //Enkidu East Bunker
      ////      Building(45).get.Faction = PlanetSideEmpire.NEUTRAL //Neti bunker
      ////      Building(46).get.Faction = PlanetSideEmpire.NEUTRAL //Hanish West Bunker
      ////      Building(47).get.Faction = PlanetSideEmpire.NEUTRAL //Irkalla East Bunker
      ////      Building(48).get.Faction = PlanetSideEmpire.NEUTRAL //Zaqar bunker
      ////      Building(49).get.Faction = PlanetSideEmpire.NEUTRAL //Kusag West Bunker
      ////      Building(50).get.Faction = PlanetSideEmpire.NEUTRAL //marduk bunker
      ////      Building(51).get.Faction = PlanetSideEmpire.TR //baal bunker
      ////      Building(52).get.Faction = PlanetSideEmpire.NEUTRAL //girru bunker
      ////      Building(53).get.Faction = PlanetSideEmpire.NEUTRAL //lahar bunker
      ////      Building(54).get.Faction = PlanetSideEmpire.NEUTRAL //akkan bunker
      //      Building(55).get.Faction = PlanetSideEmpire.VS //Irkalla_Tower
      //      Building(56).get.Faction = PlanetSideEmpire.VS //Hanish_Tower
      //      Building(57).get.Faction = PlanetSideEmpire.VS //E_Ceryshen_Warpgate_Tower
      //      Building(58).get.Faction = PlanetSideEmpire.VS //Lahar_Tower
      //      Building(59).get.Faction = PlanetSideEmpire.VS //VSSanc_Warpgate_Tower
      Building(60).get.Faction = PlanetSideEmpire.TR //Akkan_Tower
      //      Building(61).get.Faction = PlanetSideEmpire.NC //TRSanc_Warpgate_Tower
      //      Building(62).get.Faction = PlanetSideEmpire.NC //Marduk_Tower
      //      Building(63).get.Faction = PlanetSideEmpire.TR //NW_Dagon_Tower
      ////      Building(64).get.Faction = PlanetSideEmpire.NEUTRAL //E7 East Bunker (at north from bridge)
      //      Building(65).get.Faction = PlanetSideEmpire.VS //W_Hanish_Tower

      Building(5).get.ModelId = 24
      Building(6).get.ModelId = 42
      Building(7).get.ModelId = 27
      Building(8).get.ModelId = 1
      Building(9).get.ModelId = 48
      Building(10).get.ModelId = 30
      Building(11).get.ModelId = 21
      Building(12).get.ModelId = 51
      Building(13).get.ModelId = 36
      Building(14).get.ModelId = 45
      Building(15).get.ModelId = 18
      Building(16).get.ModelId = 33
      //      Building(17).get.ModelId = 57
      //      Building(18).get.ModelId = 59
      //      Building(19).get.ModelId = 61
      //      Building(20).get.ModelId = 87
      //      Building(21).get.ModelId = 71
      //      Building(22).get.ModelId = 75
      //      Building(23).get.ModelId = 64
      //      Building(24).get.ModelId = 76
      //      Building(25).get.ModelId = 74
      //      Building(26).get.ModelId = 63
      //      Building(27).get.ModelId = 72
      Building(28).get.ModelId = 70
      Building(29).get.ModelId = 58
      Building(30).get.ModelId = 80
      //      Building(31).get.ModelId = 68
      //      Building(32).get.ModelId = 66
      //      Building(33).get.ModelId = 62
      //      Building(34).get.ModelId = 60
      Building(35).get.ModelId = 69
      //      Building(36).get.ModelId = 83
      //      Building(37).get.ModelId = 55
      //      Building(38).get.ModelId = 54
      //      Building(39).get.ModelId = 67
      //      Building(40).get.ModelId = 78
      //      Building(41).get.ModelId = 85
      ////      Building(42).get.ModelId = 9
      ////      Building(43).get.ModelId = 10
      ////      Building(44).get.ModelId = 15
      ////      Building(45).get.ModelId = 11
      ////      Building(46).get.ModelId = 14
      ////      Building(47).get.ModelId = 16
      ////      Building(48).get.ModelId = 12
      ////      Building(49).get.ModelId = 17
      ////      Building(50).get.ModelId = 6
      ////      Building(51).get.ModelId = 4
      ////      Building(52).get.ModelId = 7
      ////      Building(53).get.ModelId = 8
      ////      Building(54).get.ModelId = 5
      //      Building(55).get.ModelId = 86
      //      Building(56).get.ModelId = 82
      //      Building(57).get.ModelId = 88
      //      Building(58).get.ModelId = 65
      //      Building(59).get.ModelId = 73
      Building(60).get.ModelId = 79
      //      Building(61).get.ModelId = 84
      //      Building(62).get.ModelId = 81
      //      Building(63).get.ModelId = 77
      ////      Building(64).get.ModelId = 13
      //      Building(65).get.ModelId = 56

    }
  }

  val z5 = new Zone("z5", Maps.map5, 5){
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      import net.psforever.types.PlanetSideEmpire
      Building(12).get.Faction = PlanetSideEmpire.VS //Bel
      Building(12).get.ModelId = 23
    }
  }

  val z6 = new Zone("z6", Maps.map6, 6) {
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      var silo = GUID(2094).get.asInstanceOf[ResourceSilo]
      silo.Actor ! ResourceSilo.UpdateChargeLevel(1000)

      import net.psforever.types.PlanetSideEmpire
      Building(2).get.Faction = PlanetSideEmpire.VS
      Building(2).get.ModelId = 20
      Building(38).get.ModelId = 0
      Building(42).get.ModelId = 0
      Building(48).get.Faction = PlanetSideEmpire.VS
      Building(48).get.ModelId = 59
      Building(49).get.Faction = PlanetSideEmpire.VS
      Building(49).get.ModelId = 69
    }
  }

  val z7 = new Zone("z7", Maps.map7, 7){
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      import net.psforever.types.PlanetSideEmpire
      Building(17).get.Faction = PlanetSideEmpire.NC //Ran
      Building(17).get.ModelId = 34
    }
  }

  val z8 = new Zone("z8", Maps.map8, 8)

  val z9 = new Zone("z9", Maps.map9, 9)

  val z10 = new Zone("z10", Maps.map10, 10)

  val home1 = new Zone("home1", Maps.map11, 11){
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      import net.psforever.types.PlanetSideEmpire
      Buildings.values.foreach { _.Faction = PlanetSideEmpire.NC }
    }
  }

  val home2 = new Zone("home2", Maps.map12, 12){
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      import net.psforever.types.PlanetSideEmpire
      Buildings.values.foreach { _.Faction = PlanetSideEmpire.TR }
    }
  }

  val home3 = new Zone("home3", Maps.map13, 13) {
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      import net.psforever.types.PlanetSideEmpire
      Buildings.values.foreach { _.Faction = PlanetSideEmpire.VS }
      Building(29).get.Faction = PlanetSideEmpire.NC //South Villa Gun Tower
      GUID(293).get.asInstanceOf[VehicleSpawnPad].Railed = false //building 52
      GUID(706).get.asInstanceOf[VehicleSpawnPad].Guide = List(AutoDriveControls.DistanceFromHere(50f)) //building 77
      GUID(710).get.asInstanceOf[VehicleSpawnPad].Railed = false //building 79
      GUID(712).get.asInstanceOf[VehicleSpawnPad].Railed = false //building 81
    }
  }

  val tzshtr = new Zone("tzshtr", Maps.map14, 14)

  val tzdrtr = new Zone("tzsdrtr", Maps.map15, 15)

  val tzcotr = new Zone("tzcotr", Maps.map16, 16)

  val tzshnc = new Zone("tzshnc", Maps.map14, 17)

  val tzdrnc = new Zone("tzdrnc", Maps.map15, 18)

  val tzconc = new Zone("tzconc", Maps.map16, 19)

  val tzshvs = new Zone("tzshvs", Maps.map14, 20)

  val tzdrvs = new Zone("tzdrvs", Maps.map15, 21)

  val tzcovs = new Zone("tzcovs", Maps.map16, 22)

  val c1 = new Zone("c1", Maps.ugd01, 23)

  val c2 = new Zone("c2", Maps.ugd02, 24)

  val c3 = new Zone("c3", Maps.ugd03, 25)//{
  //    override def Init(implicit context : ActorContext) : Unit = {
  //      super.Init(context)
  //
  //      import net.psforever.types.PlanetSideEmpire
  //      Building(10359).get.Faction = PlanetSideEmpire.TR //Redoubt SE
  //      Building(10359).get.ModelId = 104
  //    }
  //  }

  val c4 = new Zone("c4", Maps.ugd04, 26)

  val c5 = new Zone("c5", Maps.ugd05, 27)

  val c6 = new Zone("c6", Maps.ugd06, 28)

  val i1 = new Zone("i1", Maps.map99, 29)

  val i2 = new Zone("i2", Maps.map98, 30)

  val i3 = new Zone("i3", Maps.map97, 31)

  val i4 = new Zone("i4", Maps.map96, 32)

  /**
    * Get the zone identifier name for the sanctuary continent of a given empire.
    * @param faction the empire
    * @return the zone id, with a blank string as an invalidating result
    */
  def SanctuaryZoneId(faction : PlanetSideEmpire.Value) : String = {
    faction match {
      case PlanetSideEmpire.NC => "home1"
      case PlanetSideEmpire.TR => "home2"
      case PlanetSideEmpire.VS => "home3"
      case PlanetSideEmpire.NEUTRAL => "" //invalid, not black ops
    }
  }

  /**
    * Get the zone number for the sanctuary continent of a given empire.
    * @param faction the empire
    * @return the zone number, within the sequence 1-32, and with 0 as an invalidating result
    */
  def SanctuaryZoneNumber(faction : PlanetSideEmpire.Value) : Int = {
    faction match {
      case PlanetSideEmpire.NC => 11
      case PlanetSideEmpire.TR => 12
      case PlanetSideEmpire.VS => 13
      case PlanetSideEmpire.NEUTRAL => 0 //invalid, not black ops
    }
  }
}
