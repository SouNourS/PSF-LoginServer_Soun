// Copyright (c) 2017 PSForever
import akka.actor.ActorContext
import net.psforever.objects.GlobalDefinitions
import net.psforever.objects.serverobject.resourcesilo.ResourceSilo
import net.psforever.objects.zones.Zone
import net.psforever.types.PlanetSideEmpire

object Zones {
  val z1 = new Zone("z1", Maps.map1, 1){ // Solsar
  override def Init(implicit context : ActorContext) : Unit = {
    super.Init(context)

    Buildings.values.foreach(building =>
      building.Amenities.foreach(amenity =>
        amenity.Definition match {
          case GlobalDefinitions.resource_silo =>
            val r = new scala.util.Random
            val silo = amenity.asInstanceOf[ResourceSilo]
            val ntu: Int = 900 + r.nextInt(100) - silo.ChargeLevel
            silo.Actor ! ResourceSilo.UpdateChargeLevel(ntu)
          case _ => ;
        }
      )
    )

    BuildingByMapId(5).Name = Some("Seth")
    BuildingByMapId(6).Name = Some("Bastet")
    BuildingByMapId(7).Name = Some("Aton")
    BuildingByMapId(8).Name = Some("Hapi")
    BuildingByMapId(9).Name = Some("Thoth")
    BuildingByMapId(10).Name = Some("Mont")
    BuildingByMapId(11).Name = Some("Amun")
    BuildingByMapId(12).Name = Some("Horus")
    BuildingByMapId(13).Name = Some("Sobek")

    import net.psforever.types.PlanetSideEmpire
    Buildings.values.foreach { _.Faction = PlanetSideEmpire.TR }
  }
  }

  val z2 = new Zone("z2", Maps.map2, 2){ // Hossin
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      Buildings.values.foreach(building =>
        building.Amenities.foreach(amenity =>
          amenity.Definition match {
            case GlobalDefinitions.resource_silo =>
              val r = new scala.util.Random
              val silo = amenity.asInstanceOf[ResourceSilo]
              val ntu: Int = 900 + r.nextInt(100) - silo.ChargeLevel
              silo.Actor ! ResourceSilo.UpdateChargeLevel(ntu)
            case _ => ;
          }
        )
      )

      BuildingByMapId(5).Name = Some("Voltan")
      BuildingByMapId(6).Name = Some("Naum")
      BuildingByMapId(7).Name = Some("Zotz")
      BuildingByMapId(8).Name = Some("Acan")
      BuildingByMapId(9).Name = Some("Bitol")
      BuildingByMapId(10).Name = Some("Chac")
      BuildingByMapId(11).Name = Some("Ghanon")
      BuildingByMapId(12).Name = Some("Ixtab")
      BuildingByMapId(13).Name = Some("Kisin")
      BuildingByMapId(14).Name = Some("Mulac")
      BuildingByMapId(48).Name = Some("Hurakan")

      import net.psforever.types.PlanetSideEmpire
      Buildings.values.foreach { _.Faction = PlanetSideEmpire.TR }
    }
  }

  val z3 = new Zone("z3", Maps.map3, 3){ // Cyssor
  override def Init(implicit context : ActorContext) : Unit = {
    super.Init(context)

    Buildings.values.foreach(building =>
      building.Amenities.foreach(amenity =>
        amenity.Definition match {
          case GlobalDefinitions.resource_silo =>
            val r = new scala.util.Random
            val silo = amenity.asInstanceOf[ResourceSilo]
            val ntu: Int = 900 + r.nextInt(100) - silo.ChargeLevel
            silo.Actor ! ResourceSilo.UpdateChargeLevel(ntu)
          case _ => ;
        }
      )
    )

    BuildingByMapId(1).Name = Some("Aja")
    BuildingByMapId(2).Name = Some("Bomazi")
    BuildingByMapId(4).Name = Some("Chuku")
    BuildingByMapId(5).Name = Some("Ekera")
    BuildingByMapId(6).Name = Some("Faro")
    BuildingByMapId(7).Name = Some("Wele")
    BuildingByMapId(8).Name = Some("Itan")
    BuildingByMapId(10).Name = Some("Leza")
    BuildingByMapId(11).Name = Some("Tore")
    BuildingByMapId(12).Name = Some("Nzame")
    BuildingByMapId(14).Name = Some("Orisha")
    BuildingByMapId(15).Name = Some("Pamba")
    BuildingByMapId(16).Name = Some("Shango")
    BuildingByMapId(18).Name = Some("Gunuku")
    BuildingByMapId(19).Name = Some("Honsi")
    BuildingByMapId(20).Name = Some("Kaang")
    BuildingByMapId(21).Name = Some("Mukuru")

    import net.psforever.types.PlanetSideEmpire
    Buildings.values.foreach { _.Faction = PlanetSideEmpire.VS }

  }
  }

  val z4 = new Zone("z4", Maps.map4, 4) { // Ishundar
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      Buildings.values.foreach(building =>
        building.Amenities.foreach(amenity =>
          amenity.Definition match {
            case GlobalDefinitions.resource_silo =>
              val r = new scala.util.Random
              val silo = amenity.asInstanceOf[ResourceSilo]
              val ntu: Int = 900 + r.nextInt(100) - silo.ChargeLevel
              silo.Actor ! ResourceSilo.UpdateChargeLevel(ntu)
            case _ => ;
          }
        )
      )

      import net.psforever.types.PlanetSideEmpire

      // PTS v3
      BuildingByMapId(5).Name = Some("Akkan")
      BuildingByMapId(6).Name = Some("Baal")
      BuildingByMapId(7).Name = Some("Dagon")
      BuildingByMapId(8).Name = Some("Enkidu")
      BuildingByMapId(9).Name = Some("Girru")
      BuildingByMapId(10).Name = Some("Hanish")
      BuildingByMapId(11).Name = Some("Irkalla")
      BuildingByMapId(12).Name = Some("Kusag")
      BuildingByMapId(13).Name = Some("Lahar")
      BuildingByMapId(14).Name = Some("Marduk")
      BuildingByMapId(15).Name = Some("Neti")
      BuildingByMapId(16).Name = Some("Zaqar")

      BuildingByMapId(5).Faction = PlanetSideEmpire.TR //Akkan
      BuildingByMapId(6).Faction = PlanetSideEmpire.TR //Baal
      BuildingByMapId(7).Faction = PlanetSideEmpire.TR //Dagon
      BuildingByMapId(8).Faction = PlanetSideEmpire.NC //Enkidu
      BuildingByMapId(9).Faction = PlanetSideEmpire.VS //Girru
      BuildingByMapId(10).Faction = PlanetSideEmpire.VS //Hanish
      BuildingByMapId(11).Faction = PlanetSideEmpire.VS //Irkalla
      BuildingByMapId(12).Faction = PlanetSideEmpire.VS //Kusag
      BuildingByMapId(13).Faction = PlanetSideEmpire.VS //Lahar
      BuildingByMapId(14).Faction = PlanetSideEmpire.NC //Marduk
      BuildingByMapId(15).Faction = PlanetSideEmpire.NC //Neti
      BuildingByMapId(16).Faction = PlanetSideEmpire.NC //Zaqar
      BuildingByMapId(17).Faction = PlanetSideEmpire.NC //S_Marduk_Tower
      BuildingByMapId(18).Faction = PlanetSideEmpire.NC //W_Neti_Tower
      BuildingByMapId(19).Faction = PlanetSideEmpire.NC //W_Zaqar_Tower
      BuildingByMapId(20).Faction = PlanetSideEmpire.NC //E_Zaqar_Tower
      BuildingByMapId(21).Faction = PlanetSideEmpire.NC //NE_Neti_Tower
      BuildingByMapId(22).Faction = PlanetSideEmpire.NC //SE_Ceryshen_Warpgate_Tower
      BuildingByMapId(23).Faction = PlanetSideEmpire.VS //S_Kusag_Tower
      BuildingByMapId(24).Faction = PlanetSideEmpire.VS //NW_Kusag_Tower
      BuildingByMapId(25).Faction = PlanetSideEmpire.VS //N_Ceryshen_Warpgate_Tower
      BuildingByMapId(26).Faction = PlanetSideEmpire.VS //SE_Irkalla_Tower
      BuildingByMapId(27).Faction = PlanetSideEmpire.VS //S_Irkalla_Tower
      BuildingByMapId(28).Faction = PlanetSideEmpire.TR //NE_Enkidu_Tower
      BuildingByMapId(29).Faction = PlanetSideEmpire.NC //SE_Akkan_Tower
      BuildingByMapId(30).Faction = PlanetSideEmpire.NC //SW_Enkidu_Tower
      BuildingByMapId(31).Faction = PlanetSideEmpire.TR //E_Searhus_Warpgate_Tower
      BuildingByMapId(32).Faction = PlanetSideEmpire.TR //N_Searhus_Warpgate_Tower
      BuildingByMapId(33).Faction = PlanetSideEmpire.VS //E_Girru_Tower
      BuildingByMapId(34).Faction = PlanetSideEmpire.VS //SE_Hanish_Tower
      BuildingByMapId(35).Faction = PlanetSideEmpire.TR //SW_Hanish_Tower
      BuildingByMapId(36).Faction = PlanetSideEmpire.VS //W_Girru_Tower
      BuildingByMapId(37).Faction = PlanetSideEmpire.TR //E_Dagon_Tower
      BuildingByMapId(38).Faction = PlanetSideEmpire.TR //NE_Baal_Tower
      BuildingByMapId(39).Faction = PlanetSideEmpire.TR //SE_Baal_Tower
      BuildingByMapId(40).Faction = PlanetSideEmpire.TR //S_Dagon_Tower
      BuildingByMapId(41).Faction = PlanetSideEmpire.NC //W_Ceryshen_Warpgate_Tower
      BuildingByMapId(42).Faction = PlanetSideEmpire.NEUTRAL //dagon bunker
      BuildingByMapId(43).Faction = PlanetSideEmpire.NEUTRAL //Akkan North Bunker
      BuildingByMapId(44).Faction = PlanetSideEmpire.NEUTRAL //Enkidu East Bunker
      BuildingByMapId(45).Faction = PlanetSideEmpire.NEUTRAL //Neti bunker
      BuildingByMapId(46).Faction = PlanetSideEmpire.NEUTRAL //Hanish West Bunker
      BuildingByMapId(47).Faction = PlanetSideEmpire.NEUTRAL //Irkalla East Bunker
      BuildingByMapId(48).Faction = PlanetSideEmpire.NEUTRAL //Zaqar bunker
      BuildingByMapId(49).Faction = PlanetSideEmpire.NEUTRAL //Kusag West Bunker
      BuildingByMapId(50).Faction = PlanetSideEmpire.NEUTRAL //marduk bunker
      BuildingByMapId(51).Faction = PlanetSideEmpire.TR //baal bunker
      BuildingByMapId(52).Faction = PlanetSideEmpire.NEUTRAL //girru bunker
      BuildingByMapId(53).Faction = PlanetSideEmpire.NEUTRAL //lahar bunker
      BuildingByMapId(54).Faction = PlanetSideEmpire.NEUTRAL //akkan bunker
      BuildingByMapId(55).Faction = PlanetSideEmpire.VS //Irkalla_Tower
      BuildingByMapId(56).Faction = PlanetSideEmpire.VS //Hanish_Tower
      BuildingByMapId(57).Faction = PlanetSideEmpire.VS //E_Ceryshen_Warpgate_Tower
      BuildingByMapId(58).Faction = PlanetSideEmpire.VS //Lahar_Tower
      BuildingByMapId(59).Faction = PlanetSideEmpire.VS //VSSanc_Warpgate_Tower
      BuildingByMapId(60).Faction = PlanetSideEmpire.TR //Akkan_Tower
      BuildingByMapId(61).Faction = PlanetSideEmpire.NC //TRSanc_Warpgate_Tower
      BuildingByMapId(62).Faction = PlanetSideEmpire.NC //Marduk_Tower
      BuildingByMapId(63).Faction = PlanetSideEmpire.TR //NW_Dagon_Tower
      BuildingByMapId(64).Faction = PlanetSideEmpire.NEUTRAL //E7 East Bunker (at north from bridge)
      BuildingByMapId(65).Faction = PlanetSideEmpire.VS //W_Hanish_Tower
    }
  }

  val z5 = new Zone("z5", Maps.map5, 5){ // Forseral
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      Buildings.values.foreach(building =>
        building.Amenities.foreach(amenity =>
          amenity.Definition match {
            case GlobalDefinitions.resource_silo =>
              val r = new scala.util.Random
              val silo = amenity.asInstanceOf[ResourceSilo]
              val ntu: Int = 900 + r.nextInt(100) - silo.ChargeLevel
              silo.Actor ! ResourceSilo.UpdateChargeLevel(ntu)
            case _ => ;
          }
        )
      )

      BuildingByMapId(5).Name = Some("Ogma")
      BuildingByMapId(6).Name = Some("Neit")
      BuildingByMapId(7).Name = Some("Lugh")
      BuildingByMapId(8).Name = Some("Gwydion")
      BuildingByMapId(9).Name = Some("Dagda")
      BuildingByMapId(10).Name = Some("Pwyll")
      BuildingByMapId(11).Name = Some("Anu")
      BuildingByMapId(12).Name = Some("Bel")
      BuildingByMapId(13).Name = Some("Eadon")
      BuildingByMapId(36).Name = Some("Caer")

      import net.psforever.types.PlanetSideEmpire
      Buildings.values.foreach { _.Faction = PlanetSideEmpire.VS }
    }
  }

  val z6 = new Zone("z6", Maps.map6, 6) { // Ceryshen
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      Buildings.values.foreach(building =>
        building.Amenities.foreach(amenity =>
          amenity.Definition match {
            case GlobalDefinitions.resource_silo =>
              val r = new scala.util.Random
              val silo = amenity.asInstanceOf[ResourceSilo]
              val ntu: Int = 900 + r.nextInt(100) - silo.ChargeLevel
              silo.Actor ! ResourceSilo.UpdateChargeLevel(ntu)
            case _ => ;
          }
        )
      )

//      var silo = GUID(2094).get.asInstanceOf[ResourceSilo]
//      silo.Actor ! ResourceSilo.UpdateChargeLevel(1000)

      BuildingByMapId(1).Name = Some("Akna")
      BuildingByMapId(2).Name = Some("Angutai")
      BuildingByMapId(3).Name = Some("Igaluk")
      BuildingByMapId(4).Name = Some("Keelut")
      BuildingByMapId(5).Name = Some("Nerrivik")
      BuildingByMapId(6).Name = Some("Pinga")
      BuildingByMapId(7).Name = Some("Sedna")
      BuildingByMapId(8).Name = Some("Tarqaq")
      BuildingByMapId(9).Name = Some("Tootega")

      import net.psforever.types.PlanetSideEmpire
      Buildings.values.foreach { _.Faction = PlanetSideEmpire.VS }
//      BuildingByMapId(2).Faction = PlanetSideEmpire.VS
//      BuildingByMapId(2).ModelId = 20
//      BuildingByMapId(38).ModelId = 0
//      BuildingByMapId(42).ModelId = 0
//      BuildingByMapId(48).Faction = PlanetSideEmpire.VS
//      BuildingByMapId(48).ModelId = 59
//      BuildingByMapId(49).Faction = PlanetSideEmpire.VS
//      BuildingByMapId(49).ModelId = 69
    }
  }

  val z7 = new Zone("z7", Maps.map7, 7){ // Esamir
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      Buildings.values.foreach(building =>
        building.Amenities.foreach(amenity =>
          amenity.Definition match {
            case GlobalDefinitions.resource_silo =>
              val r = new scala.util.Random
              val silo = amenity.asInstanceOf[ResourceSilo]
              val ntu: Int = 900 + r.nextInt(100) - silo.ChargeLevel
              silo.Actor ! ResourceSilo.UpdateChargeLevel(ntu)
            case _ => ;
          }
        )
      )

      BuildingByMapId(5).Name = Some("Andvari")
      BuildingByMapId(6).Name = Some("Dagur")
      BuildingByMapId(7).Name = Some("Eisa")
      BuildingByMapId(8).Name = Some("Freyr")
      BuildingByMapId(9).Name = Some("Gjallar")
      BuildingByMapId(10).Name = Some("Helheim")
      BuildingByMapId(11).Name = Some("Kvasir")
      BuildingByMapId(12).Name = Some("Mani")
      BuildingByMapId(13).Name = Some("Nott")
      BuildingByMapId(14).Name = Some("Vidar")
      BuildingByMapId(15).Name = Some("Ymir")
      BuildingByMapId(16).Name = Some("Jarl")
      BuildingByMapId(17).Name = Some("Ran")

      import net.psforever.types.PlanetSideEmpire
      Buildings.values.foreach { _.Faction = PlanetSideEmpire.NC }

      BuildingByMapId(7).Faction = PlanetSideEmpire.TR //Eisa

    }
  }

  val z8 = new Zone("z8", Maps.map8, 8) { // Oshur
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      Buildings.values.foreach(building =>
        building.Amenities.foreach(amenity =>
          amenity.Definition match {
            case GlobalDefinitions.resource_silo =>
              val r = new scala.util.Random
              val silo = amenity.asInstanceOf[ResourceSilo]
              val ntu: Int = 900 + r.nextInt(100) - silo.ChargeLevel
              silo.Actor ! ResourceSilo.UpdateChargeLevel(ntu)
            case _ => ;
          }
        )
      )

      import net.psforever.types.PlanetSideEmpire

      BuildingByMapId(5).Name = Some("Atar")
      BuildingByMapId(6).Name = Some("Dahaka")
      BuildingByMapId(7).Name = Some("Hvar")
      BuildingByMapId(8).Name = Some("Izha")
      BuildingByMapId(9).Name = Some("Jamshid")
      BuildingByMapId(10).Name = Some("Mithra")
      BuildingByMapId(11).Name = Some("Rashnu")
      BuildingByMapId(12).Name = Some("Yazata")
      BuildingByMapId(13).Name = Some("Zal")

      BuildingByMapId(5).Faction = PlanetSideEmpire.VS //Atar
      BuildingByMapId(6).Faction = PlanetSideEmpire.NC //Dahaka
      BuildingByMapId(7).Faction = PlanetSideEmpire.NC //Hvar
      BuildingByMapId(8).Faction = PlanetSideEmpire.NC //Izha
      BuildingByMapId(9).Faction = PlanetSideEmpire.TR //Jamshid
      BuildingByMapId(10).Faction = PlanetSideEmpire.TR //Mithra
      BuildingByMapId(11).Faction = PlanetSideEmpire.TR //Rashnu
      BuildingByMapId(12).Faction = PlanetSideEmpire.VS //Yazata
      BuildingByMapId(13).Faction = PlanetSideEmpire.VS //Zal
    }
  }

  val z9 = new Zone("z9", Maps.map9, 9) { // Searhus
  override def Init(implicit context : ActorContext) : Unit = {
    super.Init(context)

    Buildings.values.foreach(building =>
      building.Amenities.foreach(amenity =>
        amenity.Definition match {
          case GlobalDefinitions.resource_silo =>
            val r = new scala.util.Random
            val silo = amenity.asInstanceOf[ResourceSilo]
            val ntu: Int = 900 + r.nextInt(100) - silo.ChargeLevel
            silo.Actor ! ResourceSilo.UpdateChargeLevel(ntu)
          case _ => ;
        }
      )
    )

    BuildingByMapId(4).Name = Some("Akua")
    BuildingByMapId(5).Name = Some("Drakulu")
    BuildingByMapId(6).Name = Some("Hiro")
    BuildingByMapId(7).Name = Some("Iva")
    BuildingByMapId(8).Name = Some("Karihi")
    BuildingByMapId(9).Name = Some("Laka")
    BuildingByMapId(10).Name = Some("Matagi")
    BuildingByMapId(11).Name = Some("Ngaru")
    BuildingByMapId(12).Name = Some("Oro")
    BuildingByMapId(13).Name = Some("Pele")
    BuildingByMapId(14).Name = Some("Rehua")
    BuildingByMapId(15).Name = Some("Sina")
    BuildingByMapId(16).Name = Some("Tara")
    BuildingByMapId(17).Name = Some("Wakea")

    import net.psforever.types.PlanetSideEmpire

    Buildings.values.foreach { _.Faction = PlanetSideEmpire.TR }

  }
  }

  val z10 = new Zone("z10", Maps.map10, 10) { // Amerish
  override def Init(implicit context : ActorContext) : Unit = {
    super.Init(context)

    Buildings.values.foreach(building =>
      building.Amenities.foreach(amenity =>
        amenity.Definition match {
          case GlobalDefinitions.resource_silo =>
            val r = new scala.util.Random
            val silo = amenity.asInstanceOf[ResourceSilo]
            val ntu: Int = 900 + r.nextInt(100) - silo.ChargeLevel
            silo.Actor ! ResourceSilo.UpdateChargeLevel(ntu)
          case _ => ;
        }
      )
    )

    import net.psforever.types.PlanetSideEmpire

    BuildingByMapId(5).Name = Some("Azeban")
    BuildingByMapId(6).Name = Some("Cetan")
    BuildingByMapId(7).Name = Some("Heyoka")
    BuildingByMapId(8).Name = Some("Ikanam")
    BuildingByMapId(9).Name = Some("Kyoi")
    BuildingByMapId(10).Name = Some("Mekala")
    BuildingByMapId(11).Name = Some("Onatha")
    BuildingByMapId(12).Name = Some("Qumu")
    BuildingByMapId(13).Name = Some("Sungrey")
    BuildingByMapId(14).Name = Some("Tumas")
    BuildingByMapId(15).Name = Some("Xelas")
    BuildingByMapId(49).Name = Some("Verica")

    Buildings.values.foreach { _.Faction = PlanetSideEmpire.NC }

  }
  }

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
//      BuildingByMapId(29).Faction = PlanetSideEmpire.NC //South Villa Gun Tower
//      GUID(293).get.asInstanceOf[VehicleSpawnPad].Railed = false //building 52
//      GUID(706).get.asInstanceOf[VehicleSpawnPad].Guide = List(AutoDriveControls.DistanceFromHere(50f)) //building 77
//      GUID(710).get.asInstanceOf[VehicleSpawnPad].Railed = false //building 79
//      GUID(712).get.asInstanceOf[VehicleSpawnPad].Railed = false //building 81
    }
  }

  val tzshtr = new Zone("tzshtr", Maps.map14, 14)

  val tzdrtr = new Zone("tzdrtr", Maps.map15, 15)

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
  //      BuildingByMapId(10359).Faction = PlanetSideEmpire.TR //Redoubt SE
  //      BuildingByMapId(10359).ModelId = 104
  //    }
  //  }

  val c4 = new Zone("c4", Maps.ugd04, 26)

  val c5 = new Zone("c5", Maps.ugd05, 27)

  val c6 = new Zone("c6", Maps.ugd06, 28)

  val i1 = new Zone("i1", Maps.map99, 29){
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      Buildings.values.foreach(building =>
        building.Amenities.foreach(amenity =>
          amenity.Definition match {
            case GlobalDefinitions.resource_silo =>
              val r = new scala.util.Random
              val silo = amenity.asInstanceOf[ResourceSilo]
              val ntu: Int = 900 + r.nextInt(100) - silo.ChargeLevel
              silo.Actor ! ResourceSilo.UpdateChargeLevel(ntu)
            case _ => ;
          }
        )
      )

      BuildingByMapId(7).Name = Some("Mithra")
      BuildingByMapId(8).Name = Some("Hvar")
      BuildingByMapId(18).Name = Some("Yazata")

      import net.psforever.types.PlanetSideEmpire
      Buildings.values.foreach { _.Faction = PlanetSideEmpire.TR}
    }
  }

  val i2 = new Zone("i2", Maps.map98, 30){
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      Buildings.values.foreach(building =>
        building.Amenities.foreach(amenity =>
          amenity.Definition match {
            case GlobalDefinitions.resource_silo =>
              val r = new scala.util.Random
              val silo = amenity.asInstanceOf[ResourceSilo]
              val ntu: Int = 900 + r.nextInt(100) - silo.ChargeLevel
              silo.Actor ! ResourceSilo.UpdateChargeLevel(ntu)
            case _ => ;
          }
        )
      )

      BuildingByMapId(7).Name = Some("Zal")
      BuildingByMapId(8).Name = Some("Rashnu")
      BuildingByMapId(39).Name = Some("Sraosha")

      import net.psforever.types.PlanetSideEmpire
      Buildings.values.foreach { _.Faction = PlanetSideEmpire.NC}
    }
  }

  val i3 = new Zone("i3", Maps.map97, 31){
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      Buildings.values.foreach(building =>
        building.Amenities.foreach(amenity =>
          amenity.Definition match {
            case GlobalDefinitions.resource_silo =>
              val r = new scala.util.Random
              val silo = amenity.asInstanceOf[ResourceSilo]
              val ntu: Int = 900 + r.nextInt(100) - silo.ChargeLevel
              silo.Actor ! ResourceSilo.UpdateChargeLevel(ntu)
            case _ => ;
          }
        )
      )

      BuildingByMapId(1).Name = Some("Dahaka")
      BuildingByMapId(2).Name = Some("Jamshid")
      BuildingByMapId(3).Name = Some("Izha")

      import net.psforever.types.PlanetSideEmpire
      Buildings.values.foreach { _.Faction = PlanetSideEmpire.VS}
    }
  }

  val i4 = new Zone("i4", Maps.map96, 32){
    override def Init(implicit context : ActorContext) : Unit = {
      super.Init(context)

      Buildings.values.foreach(building =>
        building.Amenities.foreach(amenity =>
          amenity.Definition match {
            case GlobalDefinitions.resource_silo =>
              val r = new scala.util.Random
              val silo = amenity.asInstanceOf[ResourceSilo]
              val ntu: Int = 900 + r.nextInt(100) - silo.ChargeLevel
              silo.Actor ! ResourceSilo.UpdateChargeLevel(ntu)
            case _ => ;
          }
        )
      )

      BuildingByMapId(1).Name = Some("Atar")
      BuildingByMapId(2).Name = Some("North Rim Gun Tower")
      BuildingByMapId(3).Name = Some("East Rim Gun Tower")
      BuildingByMapId(4).Name = Some("South Rim Gun Tower")
      BuildingByMapId(5).Name = Some("L8 Gate Watch Tower")
      BuildingByMapId(6).Name = Some("K14 Gate Watch Tower")

    }
  }

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
