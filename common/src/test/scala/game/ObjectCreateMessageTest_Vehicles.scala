// Copyright (c) 2017 PSForever
package game

import net.psforever.packet._
import net.psforever.packet.game.{ObjectCreateMessage, _}
import net.psforever.packet.game.objectcreate._
import net.psforever.types._
import org.specs2.mutable._
import scodec.bits._

class ObjectCreateMessageTest_Vehicles extends Specification {
  val string_fury = hex"17 50010000 A79 9D01 FBC1C 12A83 2F06 00 00 21 4400003FC00101140C800C0E40000004048F3600301900000"
  val string_ant =  hex"17 C2000000 9E0 7C01 6C2D7 65535 CA16 00 00 00 4400003FC000000"
  val string_lightning = hex"17 8b010000 df1 5a00 6c2d7 65535 ca16 00 00 00 4400003fc00101300ad8040c4000000408190b801018000002617402070000000"

  "decode (fury)" in {
    PacketCoding.DecodePacket(string_fury).require match {
      case ObjectCreateMessage(len, cls, guid, parent, data) =>
        len mustEqual 336
        cls mustEqual ObjectClass.fury
        guid mustEqual PlanetSideGUID(413)
        parent.isDefined mustEqual false
        data.isDefined mustEqual true
        data.get.isInstanceOf[VehicleData] mustEqual true
        val fury = data.get.asInstanceOf[VehicleData]
        fury.basic.pos.coord.x mustEqual 6531.961f
        fury.basic.pos.coord.y mustEqual 1872.1406f
        fury.basic.pos.coord.z mustEqual 24.734375f
        fury.basic.pos.roll mustEqual 0
        fury.basic.pos.pitch mustEqual 0
        fury.basic.pos.yaw mustEqual 33
        fury.basic.pos.vel.isDefined mustEqual false
        fury.basic.faction mustEqual PlanetSideEmpire.VS
        fury.basic.unk mustEqual 4
        fury.basic.player_guid mustEqual PlanetSideGUID(0)
        fury.health mustEqual 255
        //
        fury.mountings.isDefined mustEqual true
        fury.mountings.get.size mustEqual 1
        val mounting = fury.mountings.get.head
        mounting.objectClass mustEqual ObjectClass.fury_weapon_systema
        mounting.guid mustEqual PlanetSideGUID(400)
        mounting.parentSlot mustEqual 1
        mounting.obj.isInstanceOf[WeaponData] mustEqual true
        val weapon = mounting.obj.asInstanceOf[WeaponData]
        weapon.unk1 mustEqual 0xC
        weapon.unk2 mustEqual 0x8
        weapon.fire_mode mustEqual 0
        weapon.ammo.size mustEqual 1
        val ammo = weapon.ammo.head
        ammo.objectClass mustEqual ObjectClass.hellfire_ammo
        ammo.guid mustEqual PlanetSideGUID(432)
        ammo.parentSlot mustEqual 0
        ammo.obj.isInstanceOf[AmmoBoxData] mustEqual true
        ammo.obj.asInstanceOf[AmmoBoxData].unk mustEqual 0x8
      case _ =>
        ko
    }
  }

  "decode (ant)" in {
    PacketCoding.DecodePacket(string_ant).require match {
      case ObjectCreateMessage(len, cls, guid, parent, data) =>
        len mustEqual 194L
        cls mustEqual ObjectClass.ant
        guid mustEqual PlanetSideGUID(380)
        parent.isDefined mustEqual false
        data.isDefined mustEqual true
        data.get.isInstanceOf[ANTData] mustEqual true
        val ant = data.get.asInstanceOf[ANTData]
        ant.basic.pos.coord.x mustEqual 3674.8438f
        ant.basic.pos.coord.y mustEqual 2726.789f
        ant.basic.pos.coord.z mustEqual 91.15625f
        ant.basic.pos.roll mustEqual 0
        ant.basic.pos.pitch mustEqual 0
        ant.basic.pos.yaw mustEqual 0
        ant.basic.faction mustEqual PlanetSideEmpire.VS
        ant.basic.unk mustEqual 4
        ant.basic.player_guid mustEqual PlanetSideGUID(0)
        ant.health mustEqual 255
      case _ =>
        ko
    }
  }

  "decode (lightning)" in {
    PacketCoding.DecodePacket(string_lightning).require match {
      case ObjectCreateMessage(len, cls, guid, parent, data) =>
        len mustEqual 395L
        cls mustEqual ObjectClass.lightning
        guid mustEqual PlanetSideGUID(90)
        parent.isDefined mustEqual false
        data.isDefined mustEqual true
        data.get.isInstanceOf[VehicleData] mustEqual true
        val lightning = data.get.asInstanceOf[VehicleData]
        lightning.basic.pos.coord.x mustEqual 3674.8438f
        lightning.basic.pos.coord.y mustEqual 2726.789f
        lightning.basic.pos.coord.z mustEqual 91.15625f
        lightning.basic.pos.roll mustEqual 0
        lightning.basic.pos.pitch mustEqual 0
        lightning.basic.pos.yaw mustEqual 0
        lightning.basic.faction mustEqual PlanetSideEmpire.VS
        lightning.basic.unk mustEqual 4
        lightning.basic.player_guid mustEqual PlanetSideGUID(0)
        lightning.health mustEqual 255
        lightning.mountings.isDefined mustEqual true
        lightning.mountings.get.size mustEqual 1
        val mounting = lightning.mountings.get.head
        mounting.objectClass mustEqual ObjectClass.lightning_weapon_system
        mounting.guid mustEqual PlanetSideGUID(91)
        mounting.parentSlot mustEqual 1
        mounting.obj.isInstanceOf[WeaponData] mustEqual true
        val weapon = mounting.obj.asInstanceOf[WeaponData]
        weapon.unk1 mustEqual 0x8
        weapon.unk2 mustEqual 0x8
        weapon.fire_mode mustEqual 0
        weapon.ammo.size mustEqual 2
        //0
        var ammo = weapon.ammo.head
        ammo.objectClass mustEqual ObjectClass.bullet_75mm
        ammo.guid mustEqual PlanetSideGUID(92)
        ammo.parentSlot mustEqual 0
        ammo.obj.isInstanceOf[AmmoBoxData] mustEqual true
        ammo.obj.asInstanceOf[AmmoBoxData].unk mustEqual 0x0
        //1
        ammo = weapon.ammo(1)
        ammo.objectClass mustEqual ObjectClass.bullet_25mm
        ammo.guid mustEqual PlanetSideGUID(93)
        ammo.parentSlot mustEqual 1
        ammo.obj.isInstanceOf[AmmoBoxData] mustEqual true
        ammo.obj.asInstanceOf[AmmoBoxData].unk mustEqual 0x0
      case _ =>
        ko
    }
  }

  "encode (fury)" in {
    val obj = VehicleData(
      CommonFieldData(
        PlacementData(6531.961f, 1872.1406f, 24.734375f, 0, 0, 33),
        PlanetSideEmpire.VS, 4
      ),
      255,
      InternalSlot(
        ObjectClass.fury_weapon_systema, PlanetSideGUID(400), 1, WeaponData(
          0xC, 0x8, 0, ObjectClass.hellfire_ammo, PlanetSideGUID(432), 0, AmmoBoxData(0x8)
        )
      )
    )
    val msg = ObjectCreateMessage(ObjectClass.fury, PlanetSideGUID(413), obj)
    val pkt = PacketCoding.EncodePacket(msg).require.toByteVector

    pkt mustEqual string_fury
  }

  "encode (ant)" in {
    val obj = ANTData(
      CommonFieldData(
        PlacementData(3674.8438f, 2726.789f, 91.15625f),
        PlanetSideEmpire.VS, 4
      ),
      255
    )
    val msg = ObjectCreateMessage(ObjectClass.ant, PlanetSideGUID(380), obj)
    val pkt = PacketCoding.EncodePacket(msg).require.toByteVector

    pkt mustEqual string_ant
  }

  "encode (lightning)" in {
    val obj = VehicleData(
      CommonFieldData(
        PlacementData(3674.8438f, 2726.789f, 91.15625f),
        PlanetSideEmpire.VS, 4
      ),
      255,
      InternalSlot(ObjectClass.lightning_weapon_system, PlanetSideGUID(91), 1,
        WeaponData(8, 8, 0, ObjectClass.bullet_75mm, PlanetSideGUID(92), 0, AmmoBoxData(), ObjectClass.bullet_25mm, PlanetSideGUID(93), 1, AmmoBoxData())
      )
    )
    val msg = ObjectCreateMessage(ObjectClass.lightning, PlanetSideGUID(90), obj)
    val pkt = PacketCoding.EncodePacket(msg).require.toByteVector

    pkt mustEqual string_lightning
  }
}
