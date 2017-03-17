// Copyright (c) 2017 PSForever

import akka.actor.{Actor, ActorIdentity, ActorRef, Cancellable, Identify, MDCContextAware}
import net.psforever.packet.{PlanetSideGamePacket, _}
import net.psforever.packet.control._
import net.psforever.packet.game._
import scodec.Attempt.{Failure, Successful}
import scodec.bits._
import org.log4s.MDC
import MDCContextAware.Implicits._
import ServiceManager.Lookup
import ServiceManager2.Lookup2
import net.psforever.objects._

import net.psforever.packet.game.objectcreate._
import net.psforever.types.{ChatMessageType, TransactionType, PlanetSideEmpire, Vector3}

class WorldSessionActor extends Actor with MDCContextAware {
  private[this] val log = org.log4s.getLogger

  private case class PokeClient()

  var sessionId: Long = 0
  var leftRef: ActorRef = ActorRef.noSender
  var rightRef: ActorRef = ActorRef.noSender

  var serviceManager = Actor.noSender
  var serviceManager2 = Actor.noSender
  var chatService = Actor.noSender
  var avatarService = Actor.noSender

  var useProximityTerminalID : Option[PlanetSideGUID] = None

  var clientKeepAlive: Cancellable = null

  override def postStop() = {
    if (clientKeepAlive != null)
      clientKeepAlive.cancel()
    val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
    if (playerOpt.isDefined) {
      val player: PlayerAvatar = playerOpt.get
      avatarService ! AvatarService.unLoadMap(PlanetSideGUID(player.guid))
    }
    val name: Int = PlayerMasterList.userDissociatesCharacter(sessionId)
    // dev hack: normally, the actual player avatar persists a minute or so after the user disconnects
    PlayerMasterList.removePlayer(name)
  }

  def receive = Initializing

  def Initializing: Receive = {
    case HelloFriend(sessionId, right) =>
      this.sessionId = sessionId
      leftRef = sender()
      rightRef = right.asInstanceOf[ActorRef]

      ServiceManager.serviceManager ! Lookup("chat")
      ServiceManager2.serviceManager2 ! Lookup2("avatar")

      context.become(Started)
    case msg =>
      log.error(s"Unknown message $msg")
      context.stop(self)
  }

  def Started: Receive = {
    case ServiceManager.LookupResult(endpoint) =>
      chatService = endpoint
      log.info("Got service " + endpoint)
    case ServiceManager2.LookupResult(endpoint) =>
      avatarService = endpoint
      log.info("Got service " + endpoint)
    case ctrl@ControlPacket(_, _) =>
      handlePktContainer(ctrl)
    case game@GamePacket(_, _, _) =>
      handlePktContainer(game)
    // temporary hack to keep the client from disconnecting
    case PokeClient() =>
      sendResponse(PacketCoding.CreateGamePacket(0, KeepAliveMessage(0)))
    case ChatMessage(to, from, data) =>
      if (to.drop(6) == "local") sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_OPEN, true, from, data, None)))
      if (to.drop(6) == "squad") sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_SQUAD, true, from, data, None)))
      if (to.drop(6) == "voice") sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_VOICE, true, from, data, None)))
    case AvatarMessage(to, function, itemID, avatar_guid, pos, vel, unk1, aim_pitch, unk2, is_crouching, unk4, is_cloaking, long) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      val OnlinePlayer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(avatar_guid)
      if (playerOpt.isDefined && OnlinePlayer.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        val onlineplayer: PlayerAvatar = OnlinePlayer.get

        if(function == "unLoadMap" && PlanetSideGUID(player.guid) != avatar_guid) {
          sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(onlineplayer.guid), 0)))
        }

        if(function == "LoadMap" && PlanetSideGUID(player.guid) != avatar_guid && onlineplayer.continent == player.continent) {
          sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateMessage(0, ObjectClass.avatar, avatar_guid, CharacterData(CharacterAppearanceData(onlineplayer.getPosition, 19, onlineplayer.faction, false, 4, onlineplayer.name, onlineplayer.getExoSuitType, onlineplayer.sex, 2, 9, 1, 3, 118, 30, 32896, 65535, 2, 255, 106, 7, RibbonBars(6, 7, 8, 220)),
            onlineplayer.getMaxHealth, onlineplayer.getHealth, onlineplayer.getPersonalArmor, 1, 7, 7, onlineplayer.getMaxStamina, onlineplayer.getStamina, 28, 4, 44, 84, 104, 1900,
            List(),
            List(),
            InventoryData(true, false, false, InventoryItem(ObjectClass.repeater, PlanetSideGUID(onlineplayer.guid + 1), 0,
              WeaponData(0, ObjectClass.bullet_9mm, PlanetSideGUID(onlineplayer.guid + 2), 0, AmmoBoxData(20))) ::
              InventoryItem(ObjectClass.mini_chaingun, PlanetSideGUID(onlineplayer.guid + 3), 2,
                ConcurrentFeedWeaponData(0, AmmoBoxData(ObjectClass.bullet_9mm, PlanetSideGUID(onlineplayer.guid + 8), 0, AmmoBoxData(30)) :: AmmoBoxData(ObjectClass.bullet_9mm_AP, PlanetSideGUID(onlineplayer.guid + 9), 1, AmmoBoxData(30)) :: Nil)) ::
              //      WeaponData(0, ObjectClass.bullet_9mm, PlanetSideGUID((xGUID+4)), 0, AmmoBoxData(100))) ::
              InventoryItem(ObjectClass.chainblade, PlanetSideGUID(onlineplayer.guid + 5), 4,
                WeaponData(0, ObjectClass.melee_ammo, PlanetSideGUID(onlineplayer.guid + 6), 0, AmmoBoxData(1))) ::
              InventoryItem(ObjectClass.locker_container, PlanetSideGUID(onlineplayer.guid + 7), 5, AmmoBoxData(1)) :: Nil)))))
          sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(onlineplayer.guid),35,40))) // br40
          sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(onlineplayer.guid),36,5))) // cr5
          sendResponse(PacketCoding.CreateGamePacket(0, ObjectHeldMessage(PlanetSideGUID(onlineplayer.guid), onlineplayer.getUsedHolster, false)))
        }

        if(function == "PlayerStateMessage" && PlanetSideGUID(player.guid) != avatar_guid && onlineplayer.continent == player.continent) {
          sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateMessage(avatar_guid, pos, vel, unk1, aim_pitch, unk2, 0, is_crouching, unk4, false, is_cloaking)))
        }

        if(function == "ObjectHeld" && PlanetSideGUID(player.guid) != avatar_guid && onlineplayer.continent == player.continent) {
          sendResponse(PacketCoding.CreateGamePacket(0, ObjectHeldMessage(avatar_guid, onlineplayer.getUsedHolster, false)))
        }

        if(function == "ChangeFireState" && onlineplayer.continent == player.continent && onlineplayer.shooting) {
          sendResponse(PacketCoding.CreateGamePacket(0, ChangeFireStateMessage_Start(itemID)))
          }
        if(function == "ChangeFireState" && onlineplayer.continent == player.continent && !onlineplayer.shooting) {
          sendResponse(PacketCoding.CreateGamePacket(0, ChangeFireStateMessage_Stop(itemID)))
        }
        if(function == "PlanetsideAttribute" && PlanetSideGUID(player.guid) != avatar_guid && onlineplayer.continent == player.continent) {
          println(avatar_guid,player.guid,"toto ",unk2,long)
          sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(avatar_guid, unk2, long)))
        }
      }
    case default => failWithError(s"Invalid packet class received: $default")
  }

  def handlePkt(pkt: PlanetSidePacket): Unit = pkt match {
    case ctrl: PlanetSideControlPacket =>
      handleControlPkt(ctrl)
    case game: PlanetSideGamePacket =>
      handleGamePkt(game)
    case default => failWithError(s"Invalid packet class received: $default")
  }

  def handlePktContainer(pkt: PlanetSidePacketContainer): Unit = pkt match {
    case ctrl@ControlPacket(opcode, ctrlPkt) =>
      //      println(pkt)
      handleControlPkt(ctrlPkt)
    case game@GamePacket(opcode, seq, gamePkt) =>
      //      println(pkt)
      handleGamePkt(gamePkt)
    case default => failWithError(s"Invalid packet container class received: $default")
  }

  def handleControlPkt(pkt: PlanetSideControlPacket) = {
    //    println(pkt)
    pkt match {
      case SlottedMetaPacket(slot, subslot, innerPacket) =>
        sendResponse(PacketCoding.CreateControlPacket(SlottedMetaAck(slot, subslot)))

        PacketCoding.DecodePacket(innerPacket) match {
          case Failure(e) =>
            log.error(innerPacket.toString)
            log.error(s"Failed to decode inner packet of SlottedMetaPacket: $e")
          case Successful(v) =>
            handlePkt(v)
        }
      case sync@ControlSync(diff, unk, f1, f2, f3, f4, fa, fb) =>
        log.debug(s"SYNC: ${sync}")
        val serverTick = Math.abs(System.nanoTime().toInt) // limit the size to prevent encoding error
        sendResponse(PacketCoding.CreateControlPacket(ControlSyncResp(diff, serverTick,
          fa, fb, fb, fa)))
      case MultiPacket(packets) =>
        packets.foreach { pkt =>
          PacketCoding.DecodePacket(pkt) match {
            case Failure(e) =>
              log.error(pkt.toString)
              log.error(s"Failed to decode inner packet of MultiPacket: $e")
            case Successful(v) =>
              handlePkt(v)
          }
        }
      case MultiPacketEx(packets) =>
        packets.foreach { pkt =>
          PacketCoding.DecodePacket(pkt) match {
            case Failure(e) =>
              log.error(pkt.toString)
              log.error(s"Failed to decode inner packet of MultiPacketEx: $e")
            case Successful(v) =>
              handlePkt(v)
          }
        }
      case default =>
        log.debug(s"Unhandled ControlPacket $default")
    }
  }

  val defaultApp = CharacterAppearanceData(Vector3(3674.8438f, 2726.789f, 91.15625f), 32, PlanetSideEmpire.TR, false, 4, "TestChar", 0, 2, 2, 9, 1, 3, 118, 30, 0x8080, 0xFFFF, 2, 0, 0, 7, RibbonBars(6, 7, 8, 220))
  //xGUID+15000+(xGUID*10-(10+xGUID))+
  val defaultInv =
    InventoryItem(ObjectClass.repeater, PlanetSideGUID(1), 0,
      WeaponData(0, ObjectClass.bullet_9mm, PlanetSideGUID(2), 0, AmmoBoxData(20))) ::
      InventoryItem(ObjectClass.mini_chaingun, PlanetSideGUID(3), 2,
        ConcurrentFeedWeaponData(0, AmmoBoxData(ObjectClass.bullet_9mm, PlanetSideGUID(1693), 0, AmmoBoxData(30)) :: AmmoBoxData(ObjectClass.bullet_9mm_AP, PlanetSideGUID(1564), 1, AmmoBoxData(30)) :: Nil)) ::
      //      WeaponData(0, ObjectClass.bullet_9mm, PlanetSideGUID((xGUID+4)), 0, AmmoBoxData(100))) ::
      InventoryItem(ObjectClass.chainblade, PlanetSideGUID(5), 4,
        WeaponData(0, ObjectClass.melee_ammo, PlanetSideGUID(6), 0, AmmoBoxData(1))) ::
      InventoryItem(ObjectClass.locker_container, PlanetSideGUID(7), 5, AmmoBoxData(1)) ::
      InventoryItem(ObjectClass.shotgun_shell, PlanetSideGUID(8), 6, AmmoBoxData(25)) ::
      InventoryItem(ObjectClass.bullet_9mm, PlanetSideGUID(9), 9, AmmoBoxData(50)) ::
      InventoryItem(ObjectClass.bullet_9mm_AP, PlanetSideGUID(10), 12, AmmoBoxData(50)) ::
      InventoryItem(ObjectClass.medkit, PlanetSideGUID(11), 33, AmmoBoxData(1)) ::
      InventoryItem(ObjectClass.remote_electronics_kit, PlanetSideGUID(12), 37, REKData(8)) ::
      InventoryItem(ObjectClass.medkit, PlanetSideGUID(13), 51, AmmoBoxData(1)) ::
      InventoryItem(ObjectClass.super_medkit, PlanetSideGUID(14), 69, AmmoBoxData(1)) ::
      InventoryItem(ObjectClass.bullet_9mm_AP, PlanetSideGUID(15), 64, AmmoBoxData(50)) ::
      InventoryItem(ObjectClass.plasma_grenade, PlanetSideGUID(16), 40, WeaponData(8, ObjectClass.plasma_grenade_ammo, PlanetSideGUID(17), 0, AmmoBoxData(3))) ::
      InventoryItem(ObjectClass.jammer_grenade, PlanetSideGUID(18), 58, WeaponData(8, ObjectClass.jammer_grenade_ammo, PlanetSideGUID(19), 0, AmmoBoxData(3))) ::
      InventoryItem(ObjectClass.frag_grenade, PlanetSideGUID(20), 76, WeaponData(8, ObjectClass.frag_grenade_ammo, PlanetSideGUID(21), 0, AmmoBoxData(3))) ::
      Nil
  val defaultObj = CharacterData(
    defaultApp,
    100, 77,
    88,
    1, 7, 7,
    100, 22,
    28, 4, 44, 84, 104, 1900,
    "xpe_sanctuary_help" :: "xpe_th_firemodes" :: "used_suppressor" :: "map12" :: Nil,
    List.empty,
    InventoryData(
      true, false, false, defaultInv
    )
  )
  var objectHex = ObjectCreateMessage(0, ObjectClass.avatar, PlanetSideGUID(0), defaultObj)
  var objectHex2 = PacketCoding.EncodePacket(objectHex).require.toByteVector

  var traveler = Traveler(this)

  def handleGamePkt(pkt: PlanetSideGamePacket) = pkt match {
    case ConnectToWorldRequestMessage(server, token, majorVersion, minorVersion, revision, buildDate, unk) =>

      val clientVersion = s"Client Version: ${majorVersion}.${minorVersion}.${revision}, ${buildDate}"

      log.info(s"New world login to ${server} with Token:${token}. ${clientVersion}")


//      sendResponse(PacketCoding.CreateGamePacket(0, objectHex))
//      sendResponse(PacketCoding.CreateGamePacket(0, CharacterInfoMessage(PlanetSideZoneID(10000), 41605313, PlanetSideGUID(xGUID), false, 6404428)))

      // NOTE: PlanetSideZoneID just chooses the background
      sendResponse(PacketCoding.CreateGamePacket(0, CharacterInfoMessage(PlanetSideZoneID(1), 0, PlanetSideGUID(0), true, 0)))
    case msg@CharacterRequestMessage(charId, action) =>
      log.info("Handling " + msg)

      action match {
        case CharacterRequestAction.Delete =>
          sendResponse(PacketCoding.CreateGamePacket(0, ActionResultMessage(false, Some(1))))
        case CharacterRequestAction.Select =>
          val xGUID = sessionId.toInt+15000+(sessionId.toInt*100-(100+sessionId.toInt))

          sendResponse(PacketCoding.CreateGamePacket(0, ZonePopulationUpdateMessage(PlanetSideGUID(13), 414, 138, 0, 138, 0, 138, 0, 138, 0)))

          val userInv =
            InventoryItem(ObjectClass.repeater, PlanetSideGUID(xGUID + 1), 0,
              WeaponData(0, ObjectClass.bullet_9mm, PlanetSideGUID(xGUID + 2), 0, AmmoBoxData(20))) ::
              InventoryItem(ObjectClass.mini_chaingun, PlanetSideGUID(xGUID + 3), 2,
                ConcurrentFeedWeaponData(0, AmmoBoxData(ObjectClass.bullet_9mm, PlanetSideGUID(1693), 0, AmmoBoxData(30)) :: AmmoBoxData(ObjectClass.bullet_9mm_AP, PlanetSideGUID(1564), 1, AmmoBoxData(30)) :: Nil)) ::
              //      WeaponData(0, ObjectClass.bullet_9mm, PlanetSideGUID((xGUID+4)), 0, AmmoBoxData(100))) ::
              InventoryItem(ObjectClass.chainblade, PlanetSideGUID(xGUID + 5), 4,
                WeaponData(0, ObjectClass.melee_ammo, PlanetSideGUID(xGUID + 6), 0, AmmoBoxData(1))) ::
              InventoryItem(ObjectClass.locker_container, PlanetSideGUID(xGUID + 7), 5, AmmoBoxData(1)) ::
              InventoryItem(ObjectClass.shotgun_shell, PlanetSideGUID(xGUID + 8), 6, AmmoBoxData(25)) ::
              InventoryItem(ObjectClass.bullet_9mm, PlanetSideGUID(xGUID + 9), 9, AmmoBoxData(50)) ::
              InventoryItem(ObjectClass.bullet_9mm_AP, PlanetSideGUID(xGUID + 10), 12, AmmoBoxData(50)) ::
              InventoryItem(ObjectClass.medkit, PlanetSideGUID(xGUID + 11), 33, AmmoBoxData(1)) ::
              InventoryItem(ObjectClass.remote_electronics_kit, PlanetSideGUID(xGUID + 12), 37, REKData(8)) ::
              InventoryItem(ObjectClass.medkit, PlanetSideGUID(xGUID + 13), 51, AmmoBoxData(1)) ::
              InventoryItem(ObjectClass.super_medkit, PlanetSideGUID(xGUID + 14), 69, AmmoBoxData(1)) ::
              InventoryItem(ObjectClass.bullet_9mm_AP, PlanetSideGUID(xGUID + 15), 64, AmmoBoxData(50)) ::
              InventoryItem(ObjectClass.plasma_grenade, PlanetSideGUID(xGUID + 16), 40, WeaponData(8, ObjectClass.plasma_grenade_ammo, PlanetSideGUID(xGUID + 17), 0, AmmoBoxData(3))) ::
              InventoryItem(ObjectClass.jammer_grenade, PlanetSideGUID(xGUID + 18), 58, WeaponData(8, ObjectClass.jammer_grenade_ammo, PlanetSideGUID(xGUID + 19), 0, AmmoBoxData(3))) ::
              InventoryItem(ObjectClass.frag_grenade, PlanetSideGUID(xGUID + 20), 76, WeaponData(8, ObjectClass.frag_grenade_ammo, PlanetSideGUID(xGUID + 21), 0, AmmoBoxData(3))) ::
              Nil

          val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(xGUID)
          if (playerOpt.isDefined) {
            val player: PlayerAvatar = playerOpt.get

            sendResponse(PacketCoding.CreateGamePacket(0,
              ObjectCreateMessage(0, ObjectClass.avatar, PlanetSideGUID(player.guid), CharacterData(CharacterAppearanceData(player.getPosition, 19, player.faction, false, 4, player.name, player.getExoSuitType, player.sex, 2, 9, 1, 3, 118, 30, 32896, 65535, 2, 255, 106, 7, RibbonBars(6, 7, 8, 220)),
                player.getMaxHealth, player.getHealth, player.getPersonalArmor, 1, 7, 7, player.getMaxStamina, player.getStamina, 28, 4, 44, 84, 104, 1900,
                List(),
                List(),
                InventoryData(true, false, false, userInv)))))

            player.setEquipmentInHolster(0,Tool(1,1))
            player.setUsedHolster(0)

            var home = Zone.get("i4").get
            if(player.faction == PlanetSideEmpire.NC) {home = Zone.get("home1").get}
            if(player.faction == PlanetSideEmpire.TR) {home = Zone.get("home2").get}
            if(player.faction == PlanetSideEmpire.VS) {home = Zone.get("home3").get}
            traveler.zone = home.zonename
            player.continent = home.zonename
            Transfer.loadMap(traveler, home)
            avatarService ! AvatarService.Join(home.zonename)
            Transfer.loadSelf(traveler, sessionId, Zone.selectRandom(home))
            avatarService ! AvatarService.LoadMap(PlanetSideGUID(player.guid))
          }

          // test OrbitalShuttleTimeMsg
          sendRawResponse(hex"5b75c4020180200f8000583a80000a80e041142903820450a00e0c1140")

//                        for(nanototo <- 0 to 1024)
//                          sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(nanototo), PlanetSideEmpire.TR)))
          sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(2), PlanetSideEmpire.TR)))
          sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(29), PlanetSideEmpire.TR)))
          //              sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(1397), PlanetSideEmpire.TR)))

          sendResponse(PacketCoding.CreateGamePacket(0, TimeOfDayMessage(1191182336)))
          sendResponse(PacketCoding.CreateGamePacket(0, ContinentalLockUpdateMessage(PlanetSideGUID(11), PlanetSideEmpire.NC))) // "The NC have captured the NC Sanctuary."
          sendResponse(PacketCoding.CreateGamePacket(0, ContinentalLockUpdateMessage(PlanetSideGUID(12), PlanetSideEmpire.TR))) // "The TR have captured the TR Sanctuary."
          sendResponse(PacketCoding.CreateGamePacket(0, ContinentalLockUpdateMessage(PlanetSideGUID(13), PlanetSideEmpire.VS))) // "The VS have captured the VS Sanctuary."
          sendResponse(PacketCoding.CreateGamePacket(0, BroadcastWarpgateUpdateMessage(PlanetSideGUID(13), PlanetSideGUID(1), false, false, true))) // VS Sanctuary: Inactive Warpgate -> Broadcast Warpgate


          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(1),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(2),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(3),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(4),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(5),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(6),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(7),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(8),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(9),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(10),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(11),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(12),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(14),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(15),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(16),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(17),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(18),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(19),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(20),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(21),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(22),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(23),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(24),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(25),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(26),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(27),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(28),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(29),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(30),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(31),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(32),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(33),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(34),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(35),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(36),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(37),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(38),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(39),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(40),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(41),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(42),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(43),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(44),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(45),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(46),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(47),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(48),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(49),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(50),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(51),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(52),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(53),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(54),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(55),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(56),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(57),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(58),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(59),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(60),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(61),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(62),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(63),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(64),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(65),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(66),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(67),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(68),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(69),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(70),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(71),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(72),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(73),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(74),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(75),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(76),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(77),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(78),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(79),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(80),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(81),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(82),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(83),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(84),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(85),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(86),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(87),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(88),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(89),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(90),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(91),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(92),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(93),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(196),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(197),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(198),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(199),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(200),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(201),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(202),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(203),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(204),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(205),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(206),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(207),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(208),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(209),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(210),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(211),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(212),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(213),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(214),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13341),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13342),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13343),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13344),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13345),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13346),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13347),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13348),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13349),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13350),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13351),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13352),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13353),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13354),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13355),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13356),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13357),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13358),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13359),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13360),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
//          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13361),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.NEUTRAL,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))


          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(6), //Ceryshen
            PlanetSideGUID(2), //Anguta
            8, //80% NTU
            true, //Base hacked
            PlanetSideEmpire.NC, //Base hacked by NC
            600000, //10 minutes remaining for hack
            PlanetSideEmpire.VS, //Base owned by VS
            0, //!! Field != 0 will cause malformed packet. See class def.
            None,
            PlanetSideGeneratorState.Critical, //Generator critical
            true, //Respawn tubes destroyed
            true, //Force dome active
            16, //Tech plant lattice benefit
            0,
            Nil, //!! Field > 0 will cause malformed packet. See class def.
            0,
            false,
            8, //!! Field != 8 will cause malformed packet. See class def.
            None,
            true, //Boosted spawn room pain field
            true))) //Boosted generator room pain field

          PlayerMasterList.userClaimsCharacter(sessionId, xGUID) // ... we do this when sending a SetCurrentAvatarMessa
          sendResponse(PacketCoding.CreateGamePacket(0, SetCurrentAvatarMessage(PlanetSideGUID(xGUID), 0, 0)))

          sendResponse(PacketCoding.CreateGamePacket(0, ReplicationStreamMessage(5, Some(6), Vector(SquadListing(255))))) //clear squad list

          import scala.concurrent.duration._
          import scala.concurrent.ExecutionContext.Implicits.global
          clientKeepAlive = context.system.scheduler.schedule(0 seconds, 500 milliseconds, self, PokeClient())

          chatService ! ChatService.Join("local")
          chatService ! ChatService.Join("squad")
          chatService ! ChatService.Join("voice")

          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
            "  \\#6Welcome! The commands \\#3/zone\\#6 and \\#3/warp\\#6 are available for use.", None)))
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
            "  \\#6You can use \\#3/fly on\\#6 (or off) to fly, or \\#3/speed X\\#6 (x from 1 to 5) to run !", None)))
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
            "  \\#6You can use local chat !", None)))
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
            "  \\#6Change continent will reset your inventory !", None)))
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
            "  \\#6The \\#3/who\\#6 command dont works but give some nice info !", None)))
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_EXPANSIONS, true, "", "1 on", None)))
        case default =>
          log.error("Unsupported " + default + " in " + msg)
      }
    case msg@CharacterCreateRequestMessage(name, head, voice, gender, empire) =>
      log.info("Handling " + msg)

      //hardcoded avatar and some pertinent equipment setup
//      val avatar: PlayerAvatar = PlayerAvatar(sessionId.toInt+15000+(sessionId.toInt*100-(100+sessionId.toInt)), name, empire, gender.id, 0, 0)
      val avatar: PlayerAvatar = PlayerAvatar(sessionId.toInt+15000+(sessionId.toInt*100-(100+sessionId.toInt)), name, empire, gender.id, 0, 0)
      avatar.setExoSuitType(0)
      //init holsters
      avatar.setEquipmentInHolster(0, Tool(0, 0)) // Beamer in pistol slot 1
      avatar.setEquipmentInHolster(2, Tool(1, 1)) // Suppressor in rifle slot 1
      avatar.setEquipmentInHolster(4, Tool(2, 2)) // Force Blade in melee slot
      avatar.setUsedHolster(0) // Start with Beamer drawn
      avatar.setPosition(defaultApp.pos)
      avatar.setPitch(defaultApp.viewPitch)
      avatar.setYaw(defaultApp.viewYaw)
      avatar.redHealth = avatar.getMaxHealth
      avatar.blueArmor = avatar.getMaxPersonalArmor
      avatar.greenStamina = avatar.getMaxStamina
      avatar.setUsedHolster(0)
      //add avatar
      PlayerMasterList.addPlayer(avatar, sessionId) // If created/added when sessionId is unavailable ...

      sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateMessage(0, 121, PlanetSideGUID(avatar.guid), None, Some(CharacterData(CharacterAppearanceData(avatar.getPosition, 19, avatar.faction, false, 4, avatar.name, avatar.getExoSuitType, avatar.sex, 2, 9, 1, 3, 118, 30, 32896, 65535, 2, 255, 106, 7, RibbonBars()),
        avatar.getMaxHealth, avatar.getHealth, avatar.getPersonalArmor, 1, 7, 7, avatar.getMaxStamina, avatar.getStamina, 28, 4, 44, 84, 104, 1900,
        List(),
        List(),
        InventoryData(true, false, false, List()))))))

      sendResponse(PacketCoding.CreateGamePacket(0, ActionResultMessage(true, None)))
//      sendResponse(PacketCoding.CreateGamePacket(0, CharacterInfoMessage(PlanetSideZoneID(10000), 41605313, PlanetSideGUID(xGUID), false, 6404428)))
      sendResponse(PacketCoding.CreateGamePacket(0, CharacterInfoMessage(PlanetSideZoneID(10000), 41605314, PlanetSideGUID(sessionId.toInt+15000+(sessionId.toInt*100-(100+sessionId.toInt))), true, 0)))

    case KeepAliveMessage(code) =>
      sendResponse(PacketCoding.CreateGamePacket(0, KeepAliveMessage(0)))

    case msg@PlayerStateMessageUpstream(avatar_guid, pos, vel, unk1, aim_pitch, unk2, seq_time, unk3, is_crouching, is_jumping, unk4, is_cloaking, unk5, unk6) =>
      //      log.info("PlayerState: " + msg)
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(avatar_guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        player.setPosition(pos)
        player.setVelocity(vel)
        player.setPitch(aim_pitch)
        player.crouched = is_crouching

        if (vel.isEmpty) {
          if (!is_crouching) {
            if (player.greenStamina + 1 > player.getMaxStamina) player.greenStamina = player.getMaxStamina
            if (player.greenStamina + 1 <= player.getMaxStamina) player.greenStamina += 1
          }
          else if (is_crouching) {
            if (player.greenStamina + 2 > player.getMaxStamina) player.greenStamina = player.getMaxStamina
            if (player.greenStamina + 2 <= player.getMaxStamina) player.greenStamina += 2
          }
          sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(avatar_guid, 2, player.greenStamina)))
        }

        if (useProximityTerminalID.isDefined && player.getVelocity.isDefined){
          sendResponse(PacketCoding.CreateGamePacket(0, ProximityTerminalUseMessage(PlanetSideGUID(0), useProximityTerminalID.get, false)))
          useProximityTerminalID = None
        }
      }

      avatarService ! AvatarService.PlayerStateMessage(msg)

    case msg@ChatMsg(messagetype, has_wide_contents, recipient, contents, note_contents) =>
      // TODO: Prevents log spam, but should be handled correctly
      if (messagetype != ChatMessageType.CMT_TOGGLE_GM) {
        log.info("Chat: " + msg)
      }
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get

        if (messagetype == ChatMessageType.CMT_TOGGLESPECTATORMODE) sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(messagetype, has_wide_contents, player.name, contents, note_contents)))

        //      if(messagetype == ChatMessageType.CMT_OPEN) {
        //        sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(15000),contents.toInt,1000)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(4717), 0)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(5398), 0)))
        ////        val msg = ObjectCreateMessage(0,contents.toInt,PlanetSideGUID(4717),Some(ObjectCreateMessageParent(PlanetSideGUID(15000),1)),Some(WeaponData(7,InternalSlot(540,PlanetSideGUID(5398),0,AmmoBoxData(1)))))
        //        val msg = ObjectCreateMessage(0,ObjectClass.KATANA,PlanetSideGUID(4717),Some(ObjectCreateMessageParent(PlanetSideGUID(15000),1)),Some(WeaponData(contents.toInt,InternalSlot(540,PlanetSideGUID(5398),0,AmmoBoxData(1)))))
        //        val pkt = PacketCoding.EncodePacket(msg).require.toByteVector
        //        sendRawResponse(pkt)
        ////        InventoryItem(ObjectClass.KATANA, PlanetSideGUID((xGUID+5)), 4,
        ////          WeaponData(8, ObjectClass.MELEE_AMMO, PlanetSideGUID((xGUID+6)), 0, AmmoBoxData(1))) ::
        //      }

        //      if(messagetype == ChatMessageType.CMT_TELL) {
        //        sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(15000),recipient.toInt,contents.toInt)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_229,true,"","@CTF_FlagSpawned^@amp_station~^@Pwyll~^@comm_station_dsp~^@Bel~^15~",None)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_229,true,"","@CTF_FlagPickedUp^HenrysCat~^@TerranRepublic~^@Pwyll~",None)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_229,true,"","@CTF_FlagDropped^HenrysCat~^@TerranRepublic~^@Pwyll~",None)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_223,true,"","@CTF_Failed_SourceResecured^@NewConglomerate~^@Pwyll~",None)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_INFO,true,"","switchboard",None)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","@OptionsCullWatermarkUsage",None)))

        //        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_223,true,"","@CTF_Failed_SourceResecured^@TerranRepublic~^@Hanish~",None)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_224,false,"","@TooFastToDismount",None)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_225,false,"","@DoorWillOpenWhenShuttleReturns",None)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","",None)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","@ArmorShieldOverride",None)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","@charsaved",None)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","@SVCP_PositionInQueue^1~^1~",None)))
        //        sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","@ArmorShieldOff",None)))
        //      }

        if (messagetype == ChatMessageType.CMT_WHO || messagetype == ChatMessageType.CMT_WHO_CSR || messagetype == ChatMessageType.CMT_WHO_CR ||
          messagetype == ChatMessageType.CMT_WHO_PLATOONLEADERS || messagetype == ChatMessageType.CMT_WHO_SQUADLEADERS || messagetype == ChatMessageType.CMT_WHO_TEAMS) {
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_WHO, true, "", "That command doesn't work for now, but : ", None)))
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_WHO, true, "",
            "NC online : " + PlayerMasterList.getWorldPopulation._1, None)))
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_WHO, true, "",
            "TR online : " + PlayerMasterList.getWorldPopulation._2, None)))
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_WHO, true, "",
            "VS online : " + PlayerMasterList.getWorldPopulation._3, None)))
        }

        val (isTransfert, zone, destination) = CSRZone.read(traveler, this.sessionId, msg)
        if(isTransfert){
          avatarService ! AvatarService.unLoadMap(PlanetSideGUID(player.guid))
          avatarService ! AvatarService.LeaveAll()
          for (i <- 1 to 500) {
            val OnlinePlayer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(i.toLong)
            if (OnlinePlayer.isDefined) {
              val onlineplayer: PlayerAvatar = OnlinePlayer.get
              if(onlineplayer.guid != player.guid) sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(onlineplayer.guid), 0)))
            }
          }
          Transfer.zone(traveler, this.sessionId, Zone.get(zone).get, destination)
          avatarService ! AvatarService.Join(player.continent)
          avatarService ! AvatarService.LoadMap(PlanetSideGUID(player.guid))
        }
        CSRWarp.read(traveler, msg)


        // TODO: handle this appropriately
        if (messagetype == ChatMessageType.CMT_QUIT) {
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_QUIT,false,"","@quit_friendly",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_QUIT,false,"","@quit_5",None)))
          avatarService ! AvatarService.unLoadMap(PlanetSideGUID(player.guid))
          avatarService ! AvatarService.LeaveAll
          chatService ! ChatService.LeaveAll
          sendResponse(DropCryptoSession())
          sendResponse(DropSession(sessionId, "user quit"))
        }

        chatService ! ChatService.NewMessage(player.name, msg)

        // TODO: Depending on messagetype, may need to prepend sender's name to contents with proper spacing
        // TODO: Just replays the packet straight back to sender; actually needs to be routed to recipients!
        if (messagetype != ChatMessageType.CMT_OPEN && messagetype != ChatMessageType.CMT_VOICE && messagetype != ChatMessageType.CMT_SQUAD) sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(messagetype, has_wide_contents, recipient, contents, note_contents)))

        if (messagetype == ChatMessageType.CMT_TOGGLESPECTATORMODE) sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_TOGGLESPECTATORMODE, has_wide_contents, player.name, contents, note_contents)))
      }

    case msg@VoiceHostRequest(unk, PlanetSideGUID(player_guid), data) =>
      log.info("Player " + player_guid + " requested in-game voice chat.")
      sendResponse(PacketCoding.CreateGamePacket(0, VoiceHostKill()))

    case msg@VoiceHostInfo(player_guid, data) =>
      sendResponse(PacketCoding.CreateGamePacket(0, VoiceHostKill()))

    case msg@ChangeFireModeMessage(item_guid, fire_mode) =>
      log.info("ChangeFireMode: " + msg)

    case msg@ChangeFireStateMessage_Start(item_guid) =>
      log.info("ChangeFireState_Start: " + msg)
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        player.shooting = true
        avatarService ! AvatarService.ChangeFireState(item_guid,sessionId)
      }

    case msg@ChangeFireStateMessage_Stop(item_guid) =>
      log.info("ChangeFireState_Stop: " + msg)
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        player.shooting = false
        avatarService ! AvatarService.ChangeFireState(item_guid,sessionId)
      }

    case msg@EmoteMsg(avatar_guid, emote) =>
      log.info("Emote: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, EmoteMsg(avatar_guid, emote)))

    case msg @ DropItemMessage(item_guid) =>
      log.info("DropItem: " + msg)
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        sendResponse(PacketCoding.CreateGamePacket(0, ObjectDetachMessage(PlanetSideGUID(player.guid), item_guid, player.getPosition, 0, 0, 0)))
        sendResponse(PacketCoding.CreateGamePacket(0, DropItemMessage(item_guid)))
      }

    case msg @ PickupItemMessage(item_guid, player_guid, unk1, unk2) =>
      log.info("PickupItem: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, PickupItemMessage(item_guid, player_guid, unk1, unk2)))
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectAttachMessage(player_guid, item_guid, 250)))

    case msg @ ReloadMessage(item_guid, ammo_clip, unk1) =>
      log.info("Reload: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, ReloadMessage(item_guid, 100, unk1)))

    case msg@ObjectHeldMessage(avatar_guid, held_holsters, unk1) =>
      log.info("ObjectHeld: " + msg)
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(avatar_guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if(held_holsters != 255) {
          player.setUsedHolster(held_holsters)
        }
        else {
          player.setUsedHolster(player.getUsedHolster)
        }
        avatarService ! AvatarService.ObjectHeld(PlanetSideGUID(player.guid))
      }

    case msg@AvatarJumpMessage(state) =>
      log.info("AvatarJump: " + msg)
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (player.greenStamina - 10 <= 0) player.greenStamina = 0
        if (player.greenStamina - 10 > 0) player.greenStamina -= 10
        sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(player.guid), 2, player.greenStamina)))
      }

    case msg@ZipLineMessage(player_guid, origin_side, action, id, pos) =>
      log.info("ZipLineMessage: " + msg)
      if (!origin_side && action == 0) {
        //doing this lets you use the zip line in one direction, cant come back
        sendResponse(PacketCoding.CreateGamePacket(0, ZipLineMessage(player_guid, origin_side, action, id, pos)))
      }
      else if (!origin_side && action == 1) {
        //disembark from zipline at destination !
        sendResponse(PacketCoding.CreateGamePacket(0, ZipLineMessage(player_guid, origin_side, action, 0, pos)))
      }
      else if (!origin_side && action == 2) {
        //get off by force
        sendResponse(PacketCoding.CreateGamePacket(0, ZipLineMessage(player_guid, origin_side, action, 0, pos)))
      }
      else if (origin_side && action == 0) {
        // for teleporters & the other zipline direction
      }

    case msg@RequestDestroyMessage(object_guid) =>
      log.info("RequestDestroy: " + msg)
      // TODO: Make sure this is the correct response in all cases
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(object_guid, 0)))

    case msg@ObjectDeleteMessage(object_guid, unk1) =>
      log.info("ObjectDelete: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(object_guid, 0)))

    case msg@MoveItemMessage(item_guid, avatar_guid_1, avatar_guid_2, dest, unk1) =>
      log.info("MoveItem: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectAttachMessage(avatar_guid_1, item_guid, dest)))

    case msg@ChangeAmmoMessage(item_guid, unk1) =>
      log.info("ChangeAmmo: " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectAttachMessage(item_guid, PlanetSideGUID(1564), 0)))
      sendResponse(PacketCoding.CreateGamePacket(0, ChangeAmmoMessage(item_guid, 100)))

    case msg@UseItemMessage(avatar_guid, unk1, object_guid, unk2, unk3, unk4, unk5, unk6, unk7, unk8, itemType) =>
      log.info("UseItem: " + msg)
      // TODO: Not all fields in the response are identical to source in real packet logs (but seems to be ok)
      // TODO: Not all incoming UseItemMessage's respond with another UseItemMessage (i.e. doors only send out GenericObjectStateMsg)
      sendResponse(PacketCoding.CreateGamePacket(0, UseItemMessage(avatar_guid, unk1, object_guid, unk2, unk3, unk4, unk5, unk6, unk7, unk8, itemType)))
      if (itemType == 121) { // TODO : medkit use ?!
        val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(avatar_guid)
        if (playerOpt.isDefined) {
          val player: PlayerAvatar = playerOpt.get
          if (player.getMaxHealth - player.redHealth == 0) {
            sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_226,false,"","@HealComplete",None)))
          }
          if (player.getMaxHealth - player.redHealth <= 25 && player.getMaxHealth - player.redHealth != 0) {
            player.redHealth = player.getMaxHealth
            sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(avatar_guid, 0, player.redHealth)))
            avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid),0,player.redHealth)
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(unk1), 2)))
          }
          else if (player.getMaxHealth - player.redHealth > 25) {
            player.redHealth += 25
            sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(avatar_guid, 0, player.redHealth)))
            avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid),0,player.redHealth)
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(unk1), 2)))
          }
        }
      }
      if (unk1 == 0 && !unk3 && unk7 == 25){ // TODO: This should only actually be sent to doors upon opening; may break non-door items upon use
        sendResponse(PacketCoding.CreateGamePacket(0, GenericObjectStateMsg(object_guid, 16)))
      }

    case msg@GenericObjectStateMsg(object_guid, unk1) =>
      log.info("GenericObjectState: " + msg)

    case msg@ItemTransactionMessage(terminal_guid, transaction_type, item_page, item_name, unk1, item_guid) =>
      log.info("ItemTransaction: " + msg)
      if (transaction_type == TransactionType.Sell) {
        sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(item_guid, 0)))
        sendResponse(PacketCoding.CreateGamePacket(0, ItemTransactionResultMessage(terminal_guid, transaction_type, true)))
      }
      if (transaction_type == TransactionType.Buy) {
        val obj = AmmoBoxData(50)
        val msg = ObjectCreateMessage(0, 28, PlanetSideGUID(1280), ObjectCreateMessageParent(PlanetSideGUID(sessionId.toInt+15000+(sessionId.toInt*100-(100+sessionId.toInt))), 250), obj)
        val pkt = PacketCoding.EncodePacket(msg).require.toByteVector
        sendRawResponse(pkt)
      }
    //      if(transaction_type == TransactionType.Learn && item_name == "anti_vehicular") {
    //        sendRawResponse(hex"45e4003000")
    //      }

    case msg@WeaponDelayFireMessage(seq_time, weapon_guid) =>
      log.info("WeaponDelayFire: " + msg)

    case msg@WeaponFireMessage(seq_time, weapon_guid, projectile_guid, shot_origin, unk1, unk2, unk3, unk4, unk5, unk6, unk7) =>
      log.info("WeaponFire: " + msg)

    case msg@WeaponDryFireMessage(weapon_guid) =>
      log.info("WeaponDryFireMessage: " + msg)

    case msg@WeaponLazeTargetPositionMessage(weapon, pos1, pos2) =>
      log.info("Lazing position: " + pos2.toString)

    case msg@HitMessage(seq_time, projectile_guid, unk1, hit_info, unk2, unk3, unk4) =>
      log.info("Hit: " + msg)

    case msg@SplashHitMessage(unk1, unk2, unk3, unk4, unk5, unk6, unk7, unk8) =>
      log.info("SplashHitMessage: " + msg)

    case msg@AvatarFirstTimeEventMessage(avatar_guid, object_guid, unk1, event_name) =>
      log.info("AvatarFirstTimeEvent: " + msg)

    case msg@AvatarGrenadeStateMessage(player_guid, state) =>
      log.info("AvatarGrenadeStateMessage: " + msg)

    case msg@GenericActionMessage(action) =>
      log.info("GenericActionMessage: " + msg)

    case msg@WarpgateRequest(continent_guid, building_guid, dest_building_guid, dest_continent_guid, unk1, unk2) =>
      log.info("WarpgateRequest: " + msg)

    case msg@ProximityTerminalUseMessage(player_guid, object_guid, unk) =>
      log.info("ProximityTerminalUseMessage: " + msg)
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(player_guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if(!unk && player.getVelocity.isEmpty){
          useProximityTerminalID = Option.apply(object_guid)
          sendResponse(PacketCoding.CreateGamePacket(0, ProximityTerminalUseMessage(PlanetSideGUID(player.guid), object_guid, true)))
          if (player.redHealth + 10 > player.getMaxHealth) player.redHealth = player.getMaxHealth
          if (player.redHealth + 10 <= player.getMaxHealth) player.redHealth += 10
          if (player.blueArmor + 10 > player.getMaxPersonalArmor) player.blueArmor = player.getMaxPersonalArmor
          if (player.blueArmor + 10 <= player.getMaxPersonalArmor) player.blueArmor += 10
          sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(player.guid), 0, player.redHealth)))
          sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(player.guid), 4, player.blueArmor)))
          avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid),0,player.redHealth)
          avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid),4,player.blueArmor)
          if (player.redHealth == player.getMaxHealth && player.blueArmor == player.getMaxPersonalArmor) {
            sendResponse(PacketCoding.CreateGamePacket(0, ProximityTerminalUseMessage(PlanetSideGUID(0), object_guid, false)))
          }
        }
      }

    case msg@MountVehicleMsg(player_guid, vehicle_guid, entry_point) =>
      log.info("MounVehicleMsg: "+msg)
      sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(vehicle_guid, 0, 1000)))
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectAttachMessage(vehicle_guid, player_guid, 0)))

    case msg@DismountVehicleMsg(player_guid, u1, u2) =>
      log.info("DismountVehicleMsg: " + msg)
//      sendResponse(PacketCoding.CreateGamePacket(0, msg)) //should be safe; replace with ObjectDetachMessage later
      sendResponse(PacketCoding.CreateGamePacket(0, DismountVehicleMsg(player_guid, u1, true)))

    case msg@SquadDefinitionActionMessage(a, b, c, d, e, f, g, h, i) =>
      log.info("SquadDefinitionAction: " + msg)

    case msg@AvatarGrenadeStateMessage(player_guid, state) =>
      log.info("AvatarGrenadeStateMsg: " + msg)

    case msg@GenericCollisionMsg(u1, p, t, php, thp, pv, tv, ppos, tpos, u2, u3, u4) =>
      log.info("Ouch! " + msg)
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(p)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (player.redHealth - 10 <= 0) player.redHealth = 1
        if (player.redHealth - 10 > 0) player.redHealth -= 10
        if (player.greenStamina - 10 <= 0) player.greenStamina = 0
        if (player.greenStamina - 10 > 0) player.greenStamina -= 10
        if (player.blueArmor - 10 <= 0) player.blueArmor = 0
        if (player.blueArmor - 10 > 0) player.blueArmor -= 10
        sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(p, 0, player.redHealth)))
        sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(p, 2, player.greenStamina)))
        sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(p, 4, player.blueArmor)))
        avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid),0,player.redHealth)
        avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid),4,player.blueArmor)
      }

    case msg@BugReportMessage(version_major, version_minor, version_date, bug_type, repeatable, location, zone, pos, summary, desc) =>
      log.info("BugReportMessage: " + msg)

    case msg@BindPlayerMessage(action, bindDesc, unk1, logging, unk2, unk3, unk4, pos) =>
      log.info("BindPlayerMessage: " + msg)

    case msg@CreateShortcutMessage(player_guid, slot, unk, addShortcut, shortcut) =>
//      log.info("CreateShortcutMessage: " + msg)

    case msg @ PlanetsideAttributeMessage(avatar_guid, attribute_type, attribute_value) =>
      log.info("PlanetsideAttributeMessage: "+msg)
      sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(avatar_guid, attribute_type, attribute_value)))
      avatarService ! AvatarService.PlanetsideAttribute(avatar_guid,attribute_type,attribute_value)

    case default => log.info(s"Unhandled GamePacket ${pkt}")
  }

  def failWithError(error: String) = {
    log.error(error)
    sendResponse(PacketCoding.CreateControlPacket(ConnectionClose()))
  }

  def sendResponse(cont: PlanetSidePacketContainer): Unit = {
    log.trace("WORLD SEND: " + cont)
    sendResponse(cont.asInstanceOf[Any])
  }

  def sendResponse(msg: Any): Unit = {
    MDC("sessionId") = sessionId.toString
    rightRef !> msg
  }

  def sendRawResponse(pkt: ByteVector) = {
    log.trace("WORLD SEND RAW: " + pkt)
    sendResponse(RawPacket(pkt))
  }
}