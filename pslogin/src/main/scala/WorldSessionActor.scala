// Copyright (c) 2017 PSForever

import akka.actor.{Actor, ActorRef, Cancellable, MDCContextAware}
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
import net.psforever.types._

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


  // Info
  val lite_armor_resistance_direct : Int = 6
  val lite_armor_resistance_splash : Int = 25
  val lite_armor_resistance_aggravated : Int = 10
  val med_armor_resistance_direct : Int = 10
  val med_armor_resistance_splash : Int = 35
  val med_armor_resistance_aggravated : Int = 12
  val bullet_9mm_velocity : Int = 500
  val bullet_9mm_lifespan : Float = 0.4f
  val bullet_9mm_degrade_delay : Float = 0.15f
  val bullet_9mm_degrade_multiplier : Float = 0.25f
  val bullet_9mm_damage0 : Int = 18
  val bullet_9mm_AP_damage0 : Int = 10
  val shotgun_shell_velocity : Int = 400
  val shotgun_shell_lifespan : Float = 0.25f
  val shotgun_shell_damage0 : Int = 12
  val energy_cell_velocity : Int = 500
  val energy_cell_lifespan : Float = 0.4f
  val energy_cell_degrade_delay : Float = 0.05f
  val energy_cell_degrade_multiplier : Float = 0.4f
  val energy_cell_damage0 : Int = 18
  val lasher_projectile_velocity : Int = 120
  val lasher_projectile_lifespan : Float = 0.75f
  val lasher_projectile_degrade_delay : Float = 0.012f
  val lasher_projectile_degrade_multiplier : Float = 0.3f
  val lasher_projectile_damage0 : Int = 30
  val lasher_projectile_AP_damage0 : Int = 12
  val pulsar_projectile_velocity : Int = 500
  val pulsar_projectile_lifespan : Float = 0.4f
  val pulsar_projectile_degrade_delay : Float = 0.1f
  val pulsar_projectile_degrade_multiplier : Float = 0.4f
  val pulsar_projectile_damage0 : Int = 20
  val pulsar_projectile_AP_degrade_delay : Float = 0.1f
  val pulsar_projectile_AP_degrade_multiplier : Float = 0.5f
  val pulsar_projectile_AP_damage0 : Int = 7
  val melee_ammo_projectile_damage : Int = 25
  val melee_ammo_projectile_velocity : Int = 100
  val melee_ammo_projectile_lifespan : Float = 0.03f // test 0.02f original value
  val chainblade_projectile_damage : Int = 50
  val rocket_projectile_velocity : Int = 50
  val rocket_projectile_lifespan : Float = 8f
  val rocket_projectile_damage : Int = 50
  val rocklet_flak_projectile_velocity : Int = 60
  val rocklet_flak_projectile_lifespan : Float = 3.2f
  val rocklet_flak_projectile_damage0 : Int = 20
  val bolt_projectile_velocity : Int = 500
  val bolt_projectile_lifespan : Float = 1f
  val bolt_projectile_damage0 : Int = 100
  val flamethrower_projectile_velocity : Int = 10
  val flamethrower_projectile_lifespan : Float = 2f
  val flamethrower_projectile_degrade_delay : Float = 1f
  val flamethrower_projectile_degrade_multiplier : Float = 0.5f
  val flamethrower_projectile_damage0 : Int = 10
  val flamethrower_fireball_velocity : Int = 15
  val flamethrower_fireball_lifespan : Float = 1.2f
  val flamethrower_fireball_damage0 : Int = 30


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

      // Chat Service
      ServiceManager.serviceManager ! Lookup("chat")
      // Dunno why but cant use only 1 serviceManager
      ServiceManager2.serviceManager2 ! Lookup2("avatar")

      context.become(Started)
    case msg =>
      log.error(s"Unknown message $msg")
      context.stop(self)
  }

  def Started: Receive = {
    case ServiceManager.LookupResult(endpoint) =>
      chatService = endpoint
      log.info("ID: " + sessionId + " Got service " + endpoint)
    case ServiceManager2.LookupResult(endpoint) =>
      avatarService = endpoint
      log.info("ID: " + sessionId + " Got service " + endpoint)
    case ctrl@ControlPacket(_, _) =>
      handlePktContainer(ctrl)
    case game@GamePacket(_, _, _) =>
      handlePktContainer(game)
    // temporary hack to keep the client from disconnecting
    case PokeClient() =>
      sendResponse(PacketCoding.CreateGamePacket(0, KeepAliveMessage(0)))
    case ChatMessage(to, from, fromGUID, data) =>
      if (to.drop(6) == "local") {
        if (data.length > 1 && (data.dropRight(data.length-1) != "!" || data.drop(1).dropRight(data.length-2) == "!")) {
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_OPEN, true, from, data, None)))
        }
        else if (data.length == 1) {
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_OPEN, true, from, data, None)))
        }
      }
      if (to.drop(6) == "squad") {
        val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
        val OnlinePlayer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(fromGUID)
        if (playerOpt.isDefined && OnlinePlayer.isDefined) {
          val player: PlayerAvatar = playerOpt.get
          val onlineplayer: PlayerAvatar = OnlinePlayer.get
          if (player.faction == onlineplayer.faction) sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_SQUAD, true, from, data, None)))
        }
      }
      if (to.drop(6) == "voice") sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_VOICE, true, from, data, None)))
    case AvatarMessage(to, function, itemID, avatar_guid, pos, vel, facingYaw, facingPitch, facingUpper, is_crouching, jumping, jthrust, is_cloaked, long) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      val OnlinePlayer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(avatar_guid)
      if (playerOpt.isDefined && OnlinePlayer.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        val onlineplayer: PlayerAvatar = OnlinePlayer.get

        if(function == "unLoadMap" && PlanetSideGUID(player.guid) != avatar_guid) {
          for (id <- 1 to 23) {
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(onlineplayer.guid + id),4)))
          }
          //dispose self
          sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(onlineplayer.guid),4)))
        }

        if(function == "LoadMap" && PlanetSideGUID(player.guid) != avatar_guid && onlineplayer.continent == player.continent) {
          sendResponse(PacketCoding.CreateGamePacket(0,ObjectCreateMessage(ObjectClass.avatar,PlanetSideGUID(onlineplayer.guid),
            CharacterData(CharacterAppearanceData(PlacementData(onlineplayer.getPosition,0,0,0),
              BasicCharacterData(onlineplayer.name,onlineplayer.faction,onlineplayer.sex,1,onlineplayer.voice),3,false,false,onlineplayer.getExoSuitType,"",0,false,
              onlineplayer.getPitch.toInt,onlineplayer.getYaw.toInt,false,GrenadeState.None,false,false,false,
              RibbonBars(MeritCommendation.FanFaire2007, MeritCommendation.None, MeritCommendation.Loser, MeritCommendation.None)),
              math.ceil(2.55*onlineplayer.getHealth/onlineplayer.getMaxHealth*100).toInt,
              math.ceil(2.55*onlineplayer.getPersonalArmor/onlineplayer.getMaxPersonalArmor*100).toInt,UniformStyle.ThirdUpgrade,0,Some(ImplantEffects.NoEffects),Some(Cosmetics(false,false,false,false,false)),
              InventoryData(List.empty,false,false),DrawnSlot.None))))
//          InventoryData(List(
//            InventoryItem(InternalSlot(ObjectClass.bank,PlanetSideGUID(onlineplayer.guid + 1),0,
//              WeaponData(0,0,0,InternalSlot(ObjectClass.armor_canister,PlanetSideGUID(onlineplayer.guid + 2),0,AmmoBoxData(0))))),
//            InventoryItem(InternalSlot(ObjectClass.medicalapplicator,PlanetSideGUID(onlineplayer.guid + 3),1,
//              WeaponData(0,0,0,InternalSlot(ObjectClass.health_canister,PlanetSideGUID(onlineplayer.guid + 4),0,AmmoBoxData(0)))))),false,false)

          for (ind <- 0 to 4) {
            if (onlineplayer.getHolster(ind).getEquipment.isDefined) {
              if (!onlineplayer.getEquipmentInHolster(ind).get.getToolDefinition.isConcurrentFeed){
                sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateMessage(0,
                  onlineplayer.getEquipmentInHolster(ind).get.toolDef, PlanetSideGUID(onlineplayer.getEquipmentInHolster(ind).get.guid),
                  Some(ObjectCreateMessageParent(PlanetSideGUID(onlineplayer.guid), ind)),
                  Some(WeaponData(0, 8, onlineplayer.getEquipmentInHolster(ind).get.fireModeIndex,
                    InternalSlot(onlineplayer.getEquipmentInHolster(ind).get.getAmmoType.id, PlanetSideGUID(onlineplayer.getEquipmentInHolster(ind).get.guid + 1), 0, AmmoBoxData(8)))))))
              }
              else {
                var color : Int = 0 // TR
                if (onlineplayer.faction == PlanetSideEmpire.NC) color = 4
                if (onlineplayer.faction == PlanetSideEmpire.VS) color = 8

                sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateMessage(0,
                  onlineplayer.getEquipmentInHolster(ind).get.toolDef, PlanetSideGUID(onlineplayer.getEquipmentInHolster(ind).get.guid),
                  Some(ObjectCreateMessageParent(PlanetSideGUID(onlineplayer.guid), ind)),
                  Some(ConcurrentFeedWeaponData(color, 8,
                    onlineplayer.getEquipmentInHolster(ind).get.fireModeIndex,
                    List(InternalSlot(onlineplayer.getEquipmentInHolster(ind).get.getAmmoType.id, PlanetSideGUID(onlineplayer.getEquipmentInHolster(ind).get.guid + 1), 0, AmmoBoxData(8)),
                      InternalSlot(onlineplayer.getEquipmentInHolster(ind).get.getAmmoType.id, PlanetSideGUID(onlineplayer.getEquipmentInHolster(ind).get.guid + 16), 1, AmmoBoxData(8))))))))
              }
            }
          }
          sendResponse(PacketCoding.CreateGamePacket(0, ObjectHeldMessage(PlanetSideGUID(onlineplayer.guid), onlineplayer.getUsedHolster, false)))
        }

        if(function == "PlayerStateMessage" && PlanetSideGUID(player.guid) != avatar_guid && onlineplayer.continent == player.continent && !onlineplayer.spectator) {
          val distancePlayers = distance(player.getPosition,onlineplayer.getPosition)
          val time = System.currentTimeMillis() - player.lastSeenStreamMessage(onlineplayer.guid)
          var bool : Boolean = false
          if (player.getUsedHolster != 255) {
            if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "bolt_driver") bool = true
          }
          if ((jumping || time < 200) && distancePlayers < 400) {
            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateMessage(avatar_guid, pos, vel, facingYaw, facingPitch, facingUpper, 0, is_crouching, jumping, jthrust, is_cloaked)))
            player.lastSeenStreamMessage(onlineplayer.guid) = System.currentTimeMillis()
          }
          if (vel.isEmpty && distancePlayers < 400 && time > 2000) {
            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateMessage(avatar_guid, pos, vel, facingYaw, facingPitch, facingUpper, 0, is_crouching, jumping, jthrust, is_cloaked)))
            player.lastSeenStreamMessage(onlineplayer.guid) = System.currentTimeMillis()
          }
          else if ((distancePlayers < 30 || bool ) && time > 200 ) {
            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateMessage(avatar_guid, pos, vel, facingYaw, facingPitch, facingUpper, 0, is_crouching, jumping, jthrust, is_cloaked)))
            player.lastSeenStreamMessage(onlineplayer.guid) = System.currentTimeMillis()
          }
          else if (distancePlayers < 100 && time > 500) {
            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateMessage(avatar_guid, pos, vel, facingYaw, facingPitch, facingUpper, 0, is_crouching, jumping, jthrust, is_cloaked)))
            player.lastSeenStreamMessage(onlineplayer.guid) = System.currentTimeMillis()
          }
          else if (distancePlayers < 400 && time > 1000) {
            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateMessage(avatar_guid, pos, vel, facingYaw, facingPitch, facingUpper, 0, is_crouching, jumping, jthrust, is_cloaked)))
            player.lastSeenStreamMessage(onlineplayer.guid) = System.currentTimeMillis()
          }
          else if (distancePlayers > 400 && time > 5000) {
            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateMessage(avatar_guid, pos, vel, facingYaw, facingPitch, facingUpper, 0, is_crouching, jumping, jthrust, is_cloaked)))
            player.lastSeenStreamMessage(onlineplayer.guid) = System.currentTimeMillis()
          }
        }
        if(function == "PlayerStateMessage" && PlanetSideGUID(player.guid) != avatar_guid && onlineplayer.continent == player.continent && onlineplayer.spectator) {
          sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateMessage(avatar_guid, Vector3(2,2,2), vel, facingYaw, facingPitch, facingUpper, 0, is_crouching, jumping, jthrust, is_cloaked)))
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
          sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(avatar_guid, facingUpper, long)))
        }
        if(function == "PlanetsideAttribute" && PlanetSideGUID(player.guid) == avatar_guid) {
          sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(avatar_guid, facingUpper, long)))
        }
        if(function == "Doors" && onlineplayer.continent == player.continent) {
          if (distance(player.getPosition,onlineplayer.getPosition) < 200) {
            sendResponse(PacketCoding.CreateGamePacket(0, GenericObjectStateMsg(PlanetSideGUID(facingUpper), 16)))
            var tempo : Boolean = false
            var ind : Int = 1
            while (!tempo) {
              if (player.doors(ind) == 0) {
                player.doors(ind) = facingUpper
                player.doorsTime(ind) = System.currentTimeMillis()
                tempo = true
              }
              else {ind += 1}
            }
          }
        }
        if(function == "PlayerStateShift" && PlanetSideGUID(player.guid) == avatar_guid && player.continent == "i2") {
          if(player.faction == PlanetSideEmpire.NC) {
            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(4689, 5458, 49),0)))) // oshur
            //            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(1944, 1940, 36),0)))) // nexus
            //            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(1921, 2066, 40),0)))) // nexus
          }
          if(player.faction == PlanetSideEmpire.TR) {
            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(4694, 5399, 54),0)))) // oshur
            //            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(1966, 1959, 26),0)))) // nexus
            //            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(1888, 1872, 40),0)))) // nexus
          }
          if(player.faction == PlanetSideEmpire.VS) {
            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(4720, 5401, 46),0)))) // oshur
            //            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(2038, 1993, 31),0)))) // nexus
            //            sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(2029, 2012, 40),0)))) // nexus
          }
          sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(avatar_guid, 15, 50)))
          player.redHealth = player.getMaxHealth
          player.greenStamina = player.getMaxStamina
          player.blueArmor = player.getMaxPersonalArmor
          sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(avatar_guid, 2, player.greenStamina)))
          avatarService ! AvatarService.PlanetsideAttribute(avatar_guid,0,player.redHealth)
          avatarService ! AvatarService.PlanetsideAttribute(avatar_guid,4,player.blueArmor)
          avatarService ! AvatarService.DestroyDisplay(itemID,PlanetSideGUID(player.guid))
        }
        if(function == "DestroyDisplay") {
          val Killer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(itemID)
          if (Killer.isDefined) {
            val killer: PlayerAvatar = Killer.get
                sendResponse(PacketCoding.CreateGamePacket(0,
                  DestroyDisplayMessage(killer.name, 30981173, killer.faction, false, 121, killer.getEquipmentInHolster(killer.getUsedHolster).get.toolDef, onlineplayer.name, 31035057, onlineplayer.faction, false)))
          }
          if (player.guid == onlineplayer.guid ) sendResponse(PacketCoding.CreateGamePacket(0, AvatarDeadStateMessage(1,30000,30000,onlineplayer.getPosition,0,true)))
        }
        if(function == "HitHintReturn") {
          if (player.guid == onlineplayer.guid) {
            val Killer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(itemID)
            if (Killer.isDefined) {
              val killer: PlayerAvatar = Killer.get
              sendResponse(PacketCoding.CreateGamePacket(0, HitHint(PlanetSideGUID(killer.guid), PlanetSideGUID(player.guid))))
            }
          }
        }
        if(function == "ChangeFireMode" && PlanetSideGUID(player.guid) != avatar_guid && onlineplayer.continent == player.continent ) {
          sendResponse(PacketCoding.CreateGamePacket(0, ChangeFireModeMessage(itemID, facingYaw)))
        }
        if(function == "ChangeAmmoMode" && PlanetSideGUID(player.guid) != avatar_guid && onlineplayer.continent == player.continent ) {
          sendResponse(PacketCoding.CreateGamePacket(0, ChangeAmmoMessage(itemID, 1)))
        }
        if(function == "ReloadMsg" && PlanetSideGUID(player.guid) != avatar_guid && onlineplayer.continent == player.continent ) {
          sendResponse(PacketCoding.CreateGamePacket(0, ReloadMessage(itemID, facingYaw, 0)))
        }
        if(function == "ChangeWeapon" && PlanetSideGUID(player.guid) != avatar_guid && onlineplayer.continent == player.continent ) {
          val unk1 = facingYaw

          if (unk1 >= 10) {
            val unk = unk1 - 10

            // clean the old weapon
            for (i : Int <- 1 to 23) {
              if (i == 10) Thread.sleep(250)
              sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(onlineplayer.guid + i), 0)))
            }

            sendResponse(PacketCoding.CreateGamePacket(0, ArmorChangedMessage(PlanetSideGUID(onlineplayer.guid),onlineplayer.getExoSuitType,0)))

            for (ind <- 0 to 4) {
              if (onlineplayer.getHolster(ind).getEquipment.isDefined) {
                if (!onlineplayer.getEquipmentInHolster(ind).get.getToolDefinition.isConcurrentFeed){
                  sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateMessage(0,
                    onlineplayer.getEquipmentInHolster(ind).get.toolDef, PlanetSideGUID(onlineplayer.getEquipmentInHolster(ind).get.guid),
                    Some(ObjectCreateMessageParent(PlanetSideGUID(onlineplayer.guid), ind)),
                    Some(WeaponData(0, 8, onlineplayer.getEquipmentInHolster(ind).get.fireModeIndex,
                      InternalSlot(onlineplayer.getEquipmentInHolster(ind).get.getAmmoType.id, PlanetSideGUID(onlineplayer.getEquipmentInHolster(ind).get.guid + 1), 0, AmmoBoxData(8)))))))
                }
                else {
                  var color : Int = 0 // TR
                  if (onlineplayer.faction == PlanetSideEmpire.NC) color = 4
                  if (onlineplayer.faction == PlanetSideEmpire.VS) color = 8

                  sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateMessage(0,
                    onlineplayer.getEquipmentInHolster(ind).get.toolDef, PlanetSideGUID(onlineplayer.getEquipmentInHolster(ind).get.guid),
                    Some(ObjectCreateMessageParent(PlanetSideGUID(onlineplayer.guid), ind)),
                    Some(ConcurrentFeedWeaponData(color, 8,
                      onlineplayer.getEquipmentInHolster(ind).get.fireModeIndex,
                      List(InternalSlot(onlineplayer.getEquipmentInHolster(ind).get.getAmmoType.id, PlanetSideGUID(onlineplayer.getEquipmentInHolster(ind).get.guid + 1), 0, AmmoBoxData(8)),
                        InternalSlot(onlineplayer.getEquipmentInHolster(ind).get.getAmmoType.id, PlanetSideGUID(onlineplayer.getEquipmentInHolster(ind).get.guid + 16), 1, AmmoBoxData(8))))))))
                }
              }
            }
          }
          else {
            if (onlineplayer.getHolster(unk1).getEquipment.isDefined) {
              if (!onlineplayer.getEquipmentInHolster(unk1).get.getToolDefinition.isConcurrentFeed){
                traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectCreateMessage(0,
                  onlineplayer.getEquipmentInHolster(unk1).get.toolDef, PlanetSideGUID(onlineplayer.getEquipmentInHolster(unk1).get.guid),
                  Some(ObjectCreateMessageParent(PlanetSideGUID(onlineplayer.guid), unk1)),
                  Some(WeaponData(0, 8, onlineplayer.getEquipmentInHolster(unk1).get.fireModeIndex,
                    InternalSlot(onlineplayer.getEquipmentInHolster(unk1).get.getAmmoType.id, PlanetSideGUID(onlineplayer.getEquipmentInHolster(unk1).get.guid + 1), 0, AmmoBoxData(8)))))))
              }
              else {
                var color : Int = 0 // TR
                if (onlineplayer.faction == PlanetSideEmpire.NC) color = 4
                if (onlineplayer.faction == PlanetSideEmpire.VS) color = 8

                traveler.sendToSelf(PacketCoding.CreateGamePacket(0, ObjectCreateMessage(0,
                  onlineplayer.getEquipmentInHolster(unk1).get.toolDef, PlanetSideGUID(onlineplayer.getEquipmentInHolster(unk1).get.guid),
                  Some(ObjectCreateMessageParent(PlanetSideGUID(onlineplayer.guid), unk1)),
                  Some(ConcurrentFeedWeaponData(color, 8,
                    onlineplayer.getEquipmentInHolster(unk1).get.fireModeIndex,
                    List(InternalSlot(onlineplayer.getEquipmentInHolster(unk1).get.getAmmoType.id, PlanetSideGUID(onlineplayer.getEquipmentInHolster(unk1).get.guid + 1), 0, AmmoBoxData(8)),
                      InternalSlot(onlineplayer.getEquipmentInHolster(unk1).get.getAmmoType.id, PlanetSideGUID(onlineplayer.getEquipmentInHolster(unk1).get.guid + 16), 1, AmmoBoxData(8))))))))
              }
            }
          }
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
      //            println(pkt)
      handleControlPkt(ctrlPkt)
    case game@GamePacket(opcode, seq, gamePkt) =>
      //            println(pkt)
      handleGamePkt(gamePkt)
    case default => failWithError(s"Invalid packet container class received: $default")
  }

  def handleControlPkt(pkt: PlanetSideControlPacket) = {
    //        println(pkt)
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

  val defaultApp = CharacterAppearanceData(PlacementData(Vector3(3674.8438f, 2726.789f, 91.15625f), 0, 0, 19),
    BasicCharacterData("TestChar", PlanetSideEmpire.TR, CharacterGender.Female, 41, 1),
    3, false, false, ExoSuitType.Infiltration, "", 0, false, 0, 181, true, GrenadeState.None,
    false, false, false, RibbonBars())

  var traveler = Traveler(this)

  def handleGamePkt(pkt: PlanetSideGamePacket) = pkt match {
    case ConnectToWorldRequestMessage(server, token, majorVersion, minorVersion, revision, buildDate, unk) =>

      val clientVersion = s"Client Version: ${majorVersion}.${minorVersion}.${revision}, ${buildDate}"

      log.info("ID: " + sessionId + s" New world login to ${server} with Token:${token}. ${clientVersion}")


      // NOTE: PlanetSideZoneID just chooses the background
      //      sendResponse(PacketCoding.CreateGamePacket(0, CharacterInfoMessage(PlanetSideZoneID(1), 0, PlanetSideGUID(0), true, 0)))

      sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(ObjectClass.avatar, PlanetSideGUID(1),
        DetailedCharacterData(CharacterAppearanceData(PlacementData(Vector3(1,1,1), 0, 0, 19),
          BasicCharacterData("You can create a character", PlanetSideEmpire.TR, CharacterGender.Female, 41, 1),
          3, false, false, ExoSuitType.Agile, "", 0, false, 0, 181, true, GrenadeState.None,
          false, false, false, RibbonBars(MeritCommendation.FanFaire2007, MeritCommendation.None, MeritCommendation.Loser, MeritCommendation.None)),
          100, 90, 80, 1, 7, 7, 100, 50, 28, 4, 44, 84, 104, 1900,
          List.empty, List.empty, InventoryData(List.empty), DrawnSlot.None))))
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(ObjectClass.avatar, PlanetSideGUID(2),
        DetailedCharacterData(CharacterAppearanceData(PlacementData(Vector3(1,1,1), 0, 0, 19),
          BasicCharacterData("with your own preferences and name", PlanetSideEmpire.NC, CharacterGender.Male, 41, 1),
          3, false, false, ExoSuitType.Agile, "", 0, false, 0, 181, true, GrenadeState.None,
          false, false, false, RibbonBars(MeritCommendation.FanFaire2007, MeritCommendation.None, MeritCommendation.Loser, MeritCommendation.None)),
          100, 90, 80, 1, 7, 7, 100, 50, 28, 4, 44, 84, 104, 1900,
          List.empty, List.empty, InventoryData(List.empty), DrawnSlot.None))))
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(ObjectClass.avatar, PlanetSideGUID(3),
        DetailedCharacterData(CharacterAppearanceData(PlacementData(Vector3(1,1,1), 0, 0, 19),
          BasicCharacterData("or use these default characters", PlanetSideEmpire.VS, CharacterGender.Female, 41, 1),
          3, false, false, ExoSuitType.Infiltration, "", 0, false, 0, 181, true, GrenadeState.None,
          false, false, false, RibbonBars(MeritCommendation.FanFaire2007, MeritCommendation.None, MeritCommendation.Loser, MeritCommendation.None)),
          100, 90, 80, 1, 7, 7, 100, 50, 28, 4, 44, 84, 104, 1900,
          List.empty, List.empty, InventoryData(List.empty), DrawnSlot.None))))

      sendResponse(PacketCoding.CreateGamePacket(0, CharacterInfoMessage(0,PlanetSideZoneID(1), 1, PlanetSideGUID(1), false, 0)))
      sendResponse(PacketCoding.CreateGamePacket(0, CharacterInfoMessage(0,PlanetSideZoneID(2), 2, PlanetSideGUID(2), false, 0)))
      sendResponse(PacketCoding.CreateGamePacket(0, CharacterInfoMessage(0,PlanetSideZoneID(3), 3, PlanetSideGUID(3), true, 0)))

    case msg@CharacterRequestMessage(charId, action) =>
      log.info("ID: " + sessionId + " " + msg)

      action match {
        case CharacterRequestAction.Delete =>
          sendResponse(PacketCoding.CreateGamePacket(0, ActionResultMessage(false, Some(1))))
        case CharacterRequestAction.Select =>

          sendResponse(PacketCoding.CreateGamePacket(0, ZonePopulationUpdateMessage(PlanetSideGUID(13), 414, 138, 0, 138, 0, 138, 0, 138, 0)))

          var playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
          if (playerOpt.isEmpty) {
            // define a new Player GUID
            var nbOnlineTF : Boolean = false
            var guid : Int = 15000
            while (!nbOnlineTF) {
              val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(guid)
              if (playerOpt.isDefined || guid == 40100) {
                guid += 100
              }
              else {
                nbOnlineTF = true
              }
            }
            //hardcoded avatar and some pertinent equipment setup
            var avatar: PlayerAvatar = PlayerAvatar(guid, "Unnamed_"+sessionId, PlanetSideEmpire.TR, CharacterGender.Female, 1, 2)
            if (charId == 2) avatar = PlayerAvatar(guid, "Unnamed_"+sessionId, PlanetSideEmpire.NC, CharacterGender.Female, 1, 2)
            if (charId == 3) avatar = PlayerAvatar(guid, "Unnamed_"+sessionId, PlanetSideEmpire.VS, CharacterGender.Female, 1, 2)
            avatar.setExoSuitType(ExoSuitType.Agile)
            //init holsters
            avatar.setPosition(defaultApp.pos.coord)
            avatar.setPitch(defaultApp.pos.pitch)
            avatar.setYaw(defaultApp.pos.yaw)
            avatar.redHealth = avatar.getMaxHealth
            avatar.blueArmor = avatar.getMaxPersonalArmor
            avatar.greenStamina = avatar.getMaxStamina
            //add avatar
            PlayerMasterList.addPlayer(avatar, sessionId) // If created/added when sessionId is unavailable ...
          }
          playerOpt = PlayerMasterList.getPlayer(sessionId)
          if (playerOpt.isDefined) {
            val player: PlayerAvatar = playerOpt.get

            player.sessID = sessionId

            var home = Zone.get("i2").get
            if (player.faction == PlanetSideEmpire.NC) {
              home = Zone.get("home1").get
            }
            if (player.faction == PlanetSideEmpire.TR) {
              home = Zone.get("home2").get
            }
            if (player.faction == PlanetSideEmpire.VS) {
              home = Zone.get("home3").get
            }
            traveler.zone = home.zonename
            player.continent = home.zonename
            Transfer.loadMap(traveler, home)
            avatarService ! AvatarService.Join(home.zonename)
            Transfer.loadSelf(traveler, sessionId, Zone.selectRandom(home))
            avatarService ! AvatarService.LoadMap(PlanetSideGUID(player.guid))

            // test OrbitalShuttleTimeMsg
            sendRawResponse(hex"5b75c4020180200f8000583a80000a80e041142903820450a00e0c1140")

            //          MultiPacketEx(Vector(PacketCoding.CreateGamePacket(0,SetEmpireMessage(PlanetSideGUID(2), PlanetSideEmpire.TR)),PacketCoding.CreateGamePacket(0,SetEmpireMessage(PlanetSideGUID(2), PlanetSideEmpire.TR))))

            sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(2), PlanetSideEmpire.TR)))
            sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(29), PlanetSideEmpire.TR)))

//            sendResponse(PacketCoding.CreateGamePacket(0, TimeOfDayMessage(1191182336)))
            sendResponse(PacketCoding.CreateGamePacket(0, TimeOfDayMessage(1190964992)))
            sendResponse(PacketCoding.CreateGamePacket(0, ContinentalLockUpdateMessage(PlanetSideGUID(11), PlanetSideEmpire.NC))) // "The NC have captured the NC Sanctuary."
            sendResponse(PacketCoding.CreateGamePacket(0, ContinentalLockUpdateMessage(PlanetSideGUID(12), PlanetSideEmpire.TR))) // "The TR have captured the TR Sanctuary."
            sendResponse(PacketCoding.CreateGamePacket(0, ContinentalLockUpdateMessage(PlanetSideGUID(13), PlanetSideEmpire.VS))) // "The VS have captured the VS Sanctuary."
            sendResponse(PacketCoding.CreateGamePacket(0, BroadcastWarpgateUpdateMessage(PlanetSideGUID(13), PlanetSideGUID(1), false, false, true))) // VS Sanctuary: Inactive Warpgate -> Broadcast Warpgate

            sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12), PlanetSideGUID(1), 0, false, PlanetSideEmpire.NEUTRAL, 0, PlanetSideEmpire.TR, 0, None, PlanetSideGeneratorState.Normal, true, false, 0, 0, List(), 0, false, 8, None, false, false)))
            sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12), PlanetSideGUID(2), 0, false, PlanetSideEmpire.NEUTRAL, 0, PlanetSideEmpire.TR, 0, None, PlanetSideGeneratorState.Normal, true, false, 0, 0, List(), 0, false, 8, None, false, false)))
            sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12), PlanetSideGUID(3), 0, false, PlanetSideEmpire.NEUTRAL, 0, PlanetSideEmpire.TR, 0, None, PlanetSideGeneratorState.Normal, true, false, 0, 0, List(), 0, false, 8, None, false, false)))
            sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12), PlanetSideGUID(4), 0, false, PlanetSideEmpire.NEUTRAL, 0, PlanetSideEmpire.TR, 0, None, PlanetSideGeneratorState.Normal, false, false, 0, 0, List(), 0, false, 8, None, false, false)))
            sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12), PlanetSideGUID(5), 0, false, PlanetSideEmpire.NEUTRAL, 0, PlanetSideEmpire.TR, 0, None, PlanetSideGeneratorState.Normal, false, false, 0, 0, List(), 0, false, 8, None, false, false)))
            sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12), PlanetSideGUID(6), 0, false, PlanetSideEmpire.NEUTRAL, 0, PlanetSideEmpire.TR, 0, None, PlanetSideGeneratorState.Normal, false, false, 0, 0, List(), 0, false, 8, None, false, false)))
            sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12), PlanetSideGUID(7), 0, false, PlanetSideEmpire.NEUTRAL, 0, PlanetSideEmpire.TR, 0, None, PlanetSideGeneratorState.Normal, false, false, 0, 0, List(), 0, false, 8, None, false, false)))
            sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12), PlanetSideGUID(8), 0, false, PlanetSideEmpire.NEUTRAL, 0, PlanetSideEmpire.TR, 0, None, PlanetSideGeneratorState.Normal, false, false, 0, 0, List(), 0, false, 8, None, false, false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(9),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(10),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(11),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(12),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(13),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(14),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(15),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(16),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(17),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(18),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(19),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(20),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(21),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(22),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(23),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(24),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(25),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,true,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(26),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(27),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(28),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(29),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(30),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(31),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(32),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(33),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(34),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(35),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(36),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(37),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(38),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(39),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
            //          sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(12),PlanetSideGUID(40),0,false,PlanetSideEmpire.NEUTRAL,0,PlanetSideEmpire.TR,0,None,PlanetSideGeneratorState.Normal,false,false,0,0,List(),0,false,8,None,false,false)))
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

            sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(32), PlanetSideGUID(1), 10, false, PlanetSideEmpire.NEUTRAL, 0, PlanetSideEmpire.TR, 0, None, PlanetSideGeneratorState.Normal, true, false, 2, 0, List(), 0, false, 8, None, false, false)))
            sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(32), PlanetSideGUID(2), 0, false, PlanetSideEmpire.NEUTRAL, 0, PlanetSideEmpire.TR, 0, None, PlanetSideGeneratorState.Normal, true, false, 0, 0, List(), 0, false, 8, None, false, false)))
            sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(32), PlanetSideGUID(3), 0, false, PlanetSideEmpire.NEUTRAL, 0, PlanetSideEmpire.VS, 0, None, PlanetSideGeneratorState.Normal, true, false, 0, 0, List(), 0, false, 8, None, false, false)))
            sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(32), PlanetSideGUID(4), 0, false, PlanetSideEmpire.NEUTRAL, 0, PlanetSideEmpire.NC, 0, None, PlanetSideGeneratorState.Normal, true, false, 0, 0, List(), 0, false, 8, None, false, false)))

            sendResponse(PacketCoding.CreateGamePacket(0, BuildingInfoUpdateMessage(PlanetSideGUID(8), PlanetSideGUID(6), 10, false, PlanetSideEmpire.NEUTRAL, 0, PlanetSideEmpire.VS, 0, None, PlanetSideGeneratorState.Normal, true, false, 2, 0, List(), 0, false, 8, None, false, false)))

            PlayerMasterList.userClaimsCharacter(sessionId, player.guid) // ... we do this when sending a SetCurrentAvatarMessa
            sendResponse(PacketCoding.CreateGamePacket(0, SetCurrentAvatarMessage(PlanetSideGUID(player.guid), 0, 0)))

            sendResponse(PacketCoding.CreateGamePacket(0, ReplicationStreamMessage(5, Some(6), Vector(SquadListing(255))))) //clear squad list

          }
          import scala.concurrent.duration._
          import scala.concurrent.ExecutionContext.Implicits.global
          clientKeepAlive = context.system.scheduler.schedule(0 seconds, 1000 milliseconds, self, PokeClient())

          chatService ! ChatService.Join("local")
          chatService ! ChatService.Join("squad")
          chatService ! ChatService.Join("voice")

          Thread.sleep(200)

          //          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
          //            "  \\#6Welcome! The commands \\#3/zone\\#6 and \\#3/warp\\#6 are available for use.", None)))
          //          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
          //            "  \\#6You can use \\#3/fly on\\#6 (or off) to fly, or \\#3/speed X\\#6 (x from 1 to 5) to run !", None)))
          //          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
          //            "  \\#6You can use local chat !", None)))
          //          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
          //            "  \\#6Fight is on \\#3Oshur\\#6 ! \\#3/zone oshur\\#6 to have some fun.", None)))
          //          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
          //            "  \\#6The \\#3/who\\#6 command dont works but give some nice info !", None)))

          // Welcome messages by Nick
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
            "  \\#6Welcome to PSForever! Join us on Discord at http://chat.psforever.net", None)))
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
            "  \\#6The designated combat test area has been moved from Oshur to Ascension. Type \\#3/zone ascension\\#6 to join the fight!", None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
//            "  \\#6You can use \\#3/fly on\\#6 (or off) to fly, and \\#3/speed X\\#6 (X being 1 through 5) to run faster on the ground.", None)))
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
            "  \\#6Local chat is global to all characters, and squad chat is global to everyone on your faction.", None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
//            "  \\#6To explore the game world, the commands \\#3/zone\\#6 and \\#3/warp\\#6 are available for use.", None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
//            "  \\#6The \\#3/zone\\#6 command will take you to any map in the game. Type \\#3/zone -list\\#6 for a list of zones.", None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
//            "  \\#6The \\#3/warp\\#6 command will teleport you to a location on the current map. Type \\#3/warp -list\\#6 for a list of warps, or \\#3/warp x y z\\#6 to teleport to a \\#3/loc\\#6 or location.", None)))
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_GMBROADCAST, true, "",
            "  \\#6The \\#3/who\\#6 command will show you how many characters are online for each faction.", None)))

          Thread.sleep(200)

          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_EXPANSIONS, true, "", "1 on", None)))
        case default =>
          log.error("Unsupported " + default + " in " + msg)
      }
    case msg@CharacterCreateRequestMessage(name, head, voice, gender, empire) =>
      log.info("ID: " + sessionId + " " + msg)

      // define a new Player GUID
      var nbOnlineTF : Boolean = false
      var guid : Int = 15000
      while (!nbOnlineTF) {
        val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(guid)
        if (playerOpt.isDefined || guid == 40100) {
          guid += 100
        }
        else {
          nbOnlineTF = true
        }
      }

      //hardcoded avatar and some pertinent equipment setup
      val avatar: PlayerAvatar = PlayerAvatar(guid, name, empire, gender, head, voice)
      avatar.setExoSuitType(ExoSuitType.Agile)
      avatar.setPosition(defaultApp.pos.coord)
      avatar.setPitch(defaultApp.pos.pitch)
      avatar.setYaw(defaultApp.pos.yaw)
      avatar.redHealth = avatar.getMaxHealth
      avatar.blueArmor = avatar.getMaxPersonalArmor
      avatar.greenStamina = avatar.getMaxStamina
      //add avatar
      PlayerMasterList.addPlayer(avatar, sessionId) // If created/added when sessionId is unavailable ...

      sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(ObjectClass.avatar, PlanetSideGUID(avatar.guid),
        DetailedCharacterData(CharacterAppearanceData(PlacementData(avatar.getPosition, 0, 0, 19),
          BasicCharacterData(avatar.name, avatar.faction, avatar.sex, 41, avatar.voice),
          3, false, false, avatar.getExoSuitType, "", 0, false, 0, 181, true, GrenadeState.None,
          false, false, false, RibbonBars(MeritCommendation.FanFaire2007, MeritCommendation.None, MeritCommendation.Loser, MeritCommendation.None)),
          avatar.getMaxHealth, avatar.getHealth, avatar.getPersonalArmor, 1, 7, 7, avatar.getMaxStamina, avatar.getStamina, 28, 4, 44, 84, 104, 1900,
          List.empty, List.empty, InventoryData(List.empty), DrawnSlot.None))))

      sendResponse(PacketCoding.CreateGamePacket(0, ActionResultMessage(true, None)))
      //      sendResponse(PacketCoding.CreateGamePacket(0, CharacterInfoMessage(PlanetSideZoneID(10000), 41605313, PlanetSideGUID(xGUID), false, 6404428)))
      sendResponse(PacketCoding.CreateGamePacket(0, CharacterInfoMessage(0,PlanetSideZoneID(10000), 41605314, PlanetSideGUID(guid), true, 0)))

    case KeepAliveMessage(code) =>
      sendResponse(PacketCoding.CreateGamePacket(0, KeepAliveMessage()))

    case msg@PlayerStateMessageUpstream(avatar_guid, pos, vel, unk1, aim_pitch, unk2, seq_time, unk3, is_crouching, is_jumping, unk4, is_cloaking, unk5, unk6) =>
//      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(avatar_guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get

        player.setPosition(pos)
        player.setVelocity(vel)
        player.setPitch(aim_pitch)
        player.crouched = is_crouching

        if (player.redHealth == 0 && player.death_by > 0) {
          avatarService ! AvatarService.DestroyDisplay(PlanetSideGUID(player.death_by),PlanetSideGUID(player.guid))
          player.death_by = 0
        }

        //  tp from twr to bio
//        if (player.continent == "i2") {
//          if (distance(player.getPosition,Vector3(2257f,2140f,106f)) < 3) sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(1714, 1749, 91),0))))
//          if (distance(player.getPosition,Vector3(2029f,2331f,110f)) < 3) sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(1806, 1671, 83),0))))
//          if (distance(player.getPosition,Vector3(1894f,2057f,106f)) < 3) sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(1732, 1746, 81),0))))
//        }

        if (vel.isEmpty && player.greenStamina != 100) {
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

        for (ind <- 1 to 50) {
            if (player.doors(ind) != 0) {
              if (System.currentTimeMillis() - player.doorsTime(ind) > 5000) {
                sendResponse(PacketCoding.CreateGamePacket(0, GenericObjectStateMsg(PlanetSideGUID(player.doors(ind)), 17)))
                player.doors(ind) = 0
                player.doorsTime(ind) = 0
              }
          }
        }
      }

      avatarService ! AvatarService.PlayerStateMessage(msg)

    case msg @ BeginZoningMessage() =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " Reticulating splines ...")

    case msg @ ChildObjectStateMessage(object_guid : PlanetSideGUID, pitch : Int, yaw : Int) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg @ ProjectileStateMessage(projectile_guid, shot_pos, shot_vector, unk1, unk2, unk3, unk4, time_alive) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg @ ChatMsg(messagetype, has_wide_contents, recipient, contents, note_contents) =>
      // TODO: Prevents log spam, but should be handled correctly
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get

        if (messagetype != ChatMessageType.CMT_TOGGLE_GM) {
          log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        }

//        if(messagetype == ChatMessageType.CMT_OPEN) {
//          sendResponse(PacketCoding.CreateGamePacket(0, AvatarVehicleTimerMessage(PlanetSideGUID(player.guid),"fury",72,true)))
//          println(ServerInfo.mapRotation(System.currentTimeMillis()))
//          sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(contents.toInt - 1),PlanetSideEmpire.NEUTRAL)))
//          sendResponse(PacketCoding.CreateGamePacket(0, SetEmpireMessage(PlanetSideGUID(contents.toInt),PlanetSideEmpire.TR)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_45,true,"","@NoTell_Target",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_45,true,"","@NoChat_NoCommand",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(15000),contents.toInt,1000)))
//        }
//        if(messagetype == ChatMessageType.CMT_TELL) {
//          sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(15000),recipient.toInt,contents.toInt)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_229,true,"","@CTF_FlagSpawned^@amp_station~^@Pwyll~^@comm_station_dsp~^@Bel~^15~",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_229,true,"","@CTF_FlagPickedUp^HenrysCat~^@TerranRepublic~^@Pwyll~",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_229,true,"","@CTF_FlagDropped^HenrysCat~^@TerranRepublic~^@Pwyll~",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_223,true,"","@CTF_Failed_SourceResecured^@NewConglomerate~^@Pwyll~",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_INFO,true,"","switchboard",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","@OptionsCullWatermarkUsage",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_223,true,"","@CTF_Failed_SourceResecured^@TerranRepublic~^@Hanish~",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_224,false,"","@TooFastToDismount",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_225,false,"","@DoorWillOpenWhenShuttleReturns",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","@ArmorShieldOverride",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","@charsaved",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","@SVCP_PositionInQueue^1~^1~",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_227,false,"","@ArmorShieldOff",None)))
//        }

        if (messagetype == ChatMessageType.CMT_SUICIDE) {
          avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid), 0, 0)
          avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid), 4, 0)
          player.death_by = player.guid
          Thread.sleep(1000)
          sendResponse(PacketCoding.CreateGamePacket(0, AvatarDeadStateMessage(2,0,0,player.getPosition,0,true)))
          avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid),6,1)
        }

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

        var (isTransfert, zone, destination) = CSRZone.read(traveler, this.sessionId, msg)
        if(isTransfert){
          avatarService ! AvatarService.unLoadMap(PlanetSideGUID(player.guid))
          avatarService ! AvatarService.LeaveAll()

          Thread.sleep(1000)

          val nbOnlinePlayer : Int = PlayerMasterList.getWorldPopulation._1 + PlayerMasterList.getWorldPopulation._2 + PlayerMasterList.getWorldPopulation._3
          var guid : Int = 15000
          var j : Int = 1
          while (j != nbOnlinePlayer) {
            val OnlinePlayer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(guid)
            if (OnlinePlayer.isDefined) {
              val onlineplayer: PlayerAvatar = OnlinePlayer.get
              if (player.guid != onlineplayer.guid) {
                j += 1
                guid += 100
                sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(onlineplayer.guid), 0)))
              }
              else if (player.guid == onlineplayer.guid) {
                guid += 100
              }
            }
            else {
              guid += 100
            }
          }

//          if (zone == "oshur" || zone == "z8") {
//            if (player.faction == PlanetSideEmpire.NC) destination = (4689, 5458, 49)
//            if (player.faction == PlanetSideEmpire.TR) destination = (4694, 5399, 54)
//            if (player.faction == PlanetSideEmpire.VS) destination = (4720, 5401, 46)
//          }
          if (zone == "ascension" || zone == "i2") {
//            destination = (2082, 2191, 160) // center
//            if (player.faction == PlanetSideEmpire.NC) destination = (2238, 2133, 101) // twr
//            if (player.faction == PlanetSideEmpire.TR) destination = (2048, 2325, 105) // twr
//            if (player.faction == PlanetSideEmpire.VS) destination = (1882, 2040, 101) // twr
            if (player.faction == PlanetSideEmpire.NC) destination = (2629, 2278, 84) // base
            if (player.faction == PlanetSideEmpire.TR) destination = (1804, 2693, 82) // base
            if (player.faction == PlanetSideEmpire.VS) destination = (1728, 1740, 82) // base
          }
//          if (zone == "nexus" || zone == "i4") {
//            //          if (player.faction == PlanetSideEmpire.NC) destination = (1921, 2066, 40)
//            if (player.faction == PlanetSideEmpire.NC) destination = (1944, 1940, 36)
//            //          if (player.faction == PlanetSideEmpire.TR) destination = (1888, 1872, 40)
//            if (player.faction == PlanetSideEmpire.TR) destination = (1966, 1959, 26)
//            //          if (player.faction == PlanetSideEmpire.VS) destination = (2029, 2012, 40)
//            if (player.faction == PlanetSideEmpire.VS) destination = (2038, 1993, 31)
//          }
          Transfer.zone(traveler, this.sessionId, Zone.get(zone).get, destination)
          avatarService ! AvatarService.Join(player.continent)
          avatarService ! AvatarService.LoadMap(PlanetSideGUID(player.guid))
        }
        if (player.continent != "i2" || player.admin) CSRWarp.read(traveler, msg)

        // TODO: handle this appropriately
        if (messagetype == ChatMessageType.CMT_QUIT) {
                    sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_QUIT,false,"","@quit_friendly",None)))
//          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_QUIT,false,"","@quit_5",None)))
          Thread.sleep(1000)
          avatarService ! AvatarService.unLoadMap(PlanetSideGUID(player.guid))
          avatarService ! AvatarService.LeaveAll
          chatService ! ChatService.LeaveAll
          sendResponse(DropCryptoSession())
          sendResponse(DropSession(sessionId, "user quit"))
        }

        chatService ! ChatService.NewMessage(player.name,player.guid, msg)

        // TODO: Depending on messagetype, may need to prepend sender's name to contents with proper spacing
        // TODO: Just replays the packet straight back to sender; actually needs to be routed to recipients!
        if (messagetype != ChatMessageType.CMT_OPEN &&
          messagetype != ChatMessageType.CMT_VOICE &&
          messagetype != ChatMessageType.CMT_SQUAD &&
          messagetype != ChatMessageType.CMT_TOGGLE_GM &&
          messagetype != ChatMessageType.CMT_FLY &&
          messagetype != ChatMessageType.CMT_SPEED &&
          messagetype != ChatMessageType.CMT_TELL &&
          messagetype != ChatMessageType.CMT_NOTE) sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(messagetype, has_wide_contents, recipient, contents, note_contents)))

        if ((messagetype == ChatMessageType.CMT_FLY || messagetype == ChatMessageType.CMT_SPEED) && player.continent != "i2" ) sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(messagetype, has_wide_contents, recipient, contents, note_contents)))
        if ((messagetype == ChatMessageType.CMT_FLY || messagetype == ChatMessageType.CMT_SPEED) && player.continent == "i2" && player.spectator) sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(messagetype, has_wide_contents, recipient, contents, note_contents)))

        if (messagetype == ChatMessageType.CMT_TOGGLESPECTATORMODE) {
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_TOGGLESPECTATORMODE, has_wide_contents, player.name, "off", note_contents)))
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_TELL, has_wide_contents, "Server", "Please use /t spectator on (or off)", note_contents)))
        }

        if (messagetype == ChatMessageType.CMT_NOTE) {
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.U_CMT_GMTELLFROM, has_wide_contents, "Server", "Why do you try to /note ? That's a GM command !", None)))
        }

        if (messagetype == ChatMessageType.CMT_OPEN) {
          if (contents.length > 1 && contents.dropRight(contents.length - 1) == "!" && contents.drop(1).dropRight(contents.length - 2) != "!") {
            
            if(contents.drop(1) == "list" && !player.admin) sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_TELL, has_wide_contents, "Server", "You need the admin password ;)", note_contents)))
            if(contents.drop(1) == "list" && contents.length == 5 && player.admin) {
              val nbOnlinePlayer : Int = PlayerMasterList.getWorldPopulation._1 + PlayerMasterList.getWorldPopulation._2 + PlayerMasterList.getWorldPopulation._3
              var guid : Int = 15000
              var j : Int = 1
              while (j != nbOnlinePlayer) {
                val OnlinePlayer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(guid)
                if (OnlinePlayer.isDefined) {
                  val onlineplayer: PlayerAvatar = OnlinePlayer.get
                  if (player.guid != onlineplayer.guid) {
                    j += 1
                    guid += 100
                    sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_TELL, has_wide_contents, "Server",
                      "ID / Name: " + onlineplayer.sessID + " / " + onlineplayer.name + " (" + onlineplayer.faction + ") " +
                        onlineplayer.continent + "-" + onlineplayer.posX.toInt + "/" + onlineplayer.posY.toInt + "/" + onlineplayer.posZ.toInt, note_contents)))
                  }
                  else if (player.guid == onlineplayer.guid) {
                    guid += 100
                  }
                }
                else {
                  guid += 100
                }
              }
            }
            if(contents.drop(1).dropRight(contents.length - contents.indexOf(" ")) == "list" && contents.length > 6 && player.admin) {
              val search : String = contents.drop(contents.indexOf(" ") + 1)
              val nbOnlinePlayer : Int = PlayerMasterList.getWorldPopulation._1 + PlayerMasterList.getWorldPopulation._2 + PlayerMasterList.getWorldPopulation._3
              var guid : Int = 15000
              var j : Int = 1
              while (j != nbOnlinePlayer) {
                val OnlinePlayer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(guid)
                if (OnlinePlayer.isDefined) {
                  val onlineplayer: PlayerAvatar = OnlinePlayer.get
                  if (player.guid != onlineplayer.guid) {
                    j += 1
                    guid += 100
                    if (onlineplayer.name.indexOf(search) != -1) {
                      sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_TELL, has_wide_contents, "Server",
                        "ID / Name: " + onlineplayer.sessID + " / " + onlineplayer.name + " (" + onlineplayer.faction + ") " +
                          onlineplayer.continent + "-" + onlineplayer.posX.toInt + "/" + onlineplayer.posY.toInt + "/" + onlineplayer.posZ.toInt, note_contents)))
                    }
                  }
                  else if (player.guid == onlineplayer.guid) {
                    guid += 100
                  }
                }
                else {
                  guid += 100
                }
              }
            }
            if(contents.drop(1).dropRight(contents.length - contents.indexOf(" ")) == "log" && player.admin) {
              val command : String = contents.drop(contents.indexOf(" ") + 1)
              if (command == "on") ServerInfo.setLog(true)
              if (command == "off") ServerInfo.setLog(false)
            }
            if(contents.drop(1).dropRight(contents.length - contents.indexOf(" ")) == "kick" && player.admin) {
              val sess : Long = contents.drop(contents.indexOf(" ") + 1).toLong
              val OnlinePlayer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sess)
              if (OnlinePlayer.isDefined) {
                val onlineplayer: PlayerAvatar = OnlinePlayer.get
                if (player.guid != onlineplayer.guid) {
                  avatarService ! AvatarService.unLoadMap(PlanetSideGUID(onlineplayer.guid))
                  sendResponse(DropSession(sess, "Dropped from IG admin"))
                }
                else {
                  sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_TELL, has_wide_contents, "Server", "Do you really want kick yourself ?", note_contents)))
                }
              }
              else {
                sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_TELL, has_wide_contents, "Server", "That ID do not exist !", note_contents)))
              }
            }
          }
        }

        if (messagetype == ChatMessageType.CMT_TELL) {
          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.U_CMT_TELLFROM, has_wide_contents, recipient, contents, note_contents)))
        }

        if (messagetype == ChatMessageType.CMT_TELL && recipient == "open") {
          sendResponse(PacketCoding.CreateGamePacket(0, GenericObjectStateMsg(PlanetSideGUID(contents.toInt), 16)))
        }
        if (messagetype == ChatMessageType.CMT_TELL && recipient == "close") {
          sendResponse(PacketCoding.CreateGamePacket(0, GenericObjectStateMsg(PlanetSideGUID(contents.toInt), 17)))
        }

        if (messagetype == ChatMessageType.CMT_TELL && recipient == "spectator") {
          //        if (messagetype == ChatMessageType.CMT_TOGGLESPECTATORMODE) {
          //          sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_TOGGLESPECTATORMODE, has_wide_contents, player.name, contents, note_contents)))
          if(contents == "on"){
            player.spectator = true
            sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_TELL, has_wide_contents, "spectator", "Activated", note_contents)))
            // TODO send objectdelete to others & stop sync upstream
          }
          if(contents == "off") {
            player.spectator = false
            sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_TELL, has_wide_contents, "spectator", "Deactivated", note_contents)))
            sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_FLY, true, "", "off", None)))
            sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_SPEED, true, "", "1", None)))
            player.redHealth = player.getMaxHealth
            player.blueArmor = player.getMaxPersonalArmor
            avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid), 0, player.redHealth)
            avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid), 4, player.blueArmor)
            if(player.faction == PlanetSideEmpire.NC && player.continent == "i2" && player.name != "Sounours") {
              sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(2238, 2133, 101),0))))
            }
            if(player.faction == PlanetSideEmpire.TR && player.continent == "i2" && player.name != "Sounours") {
              sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(2048, 2325, 105),0))))
            }
            if(player.faction == PlanetSideEmpire.VS && player.continent == "i2" && player.name != "Sounours") {
              sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(1882, 2040, 101),0))))
            }
            // TODO send objectcreate to others & start sync upstream
          }
        }
      }

    case msg@VoiceHostRequest(unk, PlanetSideGUID(player_guid), data) =>
      if (ServerInfo.getLog) log.info("Player " + player_guid + " requested in-game voice chat.")
      sendResponse(PacketCoding.CreateGamePacket(0, VoiceHostKill()))

    case msg@VoiceHostInfo(player_guid, data) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, VoiceHostKill()))

    case msg@ChangeFireModeMessage(item_guid, fire_mode) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        avatarService ! AvatarService.ChangeFireMode(item_guid, fire_mode, sessionId)
        player.getEquipmentInHolster(player.getUsedHolster).get.setFireModeIndex(fire_mode)
//        if (item_guid.guid == player.guid + 1)  player.getEquipmentInHolster(0).get.setFireModeIndex(fire_mode)
//        else if (item_guid.guid == player.guid + 2)  player.getEquipmentInHolster(1).get.setFireModeIndex(fire_mode)
//        else if (item_guid.guid == player.guid + 5)  player.getEquipmentInHolster(2).get.setFireModeIndex(fire_mode)
//        else if (item_guid.guid == player.guid + 7)  player.getEquipmentInHolster(4).get.setFireModeIndex(fire_mode)
      }


    case msg@ChangeFireStateMessage_Start(item_guid) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        player.shooting = true
        avatarService ! AvatarService.ChangeFireState(item_guid,sessionId)
      }

    case msg@ChangeFireStateMessage_Stop(item_guid) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        player.shooting = false
        player.lastShotSeq_time = -1
        avatarService ! AvatarService.ChangeFireState(item_guid,sessionId)
      }

    case msg@EmoteMsg(avatar_guid, emote) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, EmoteMsg(avatar_guid, emote)))

    case msg @ DropItemMessage(item_guid) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        sendResponse(PacketCoding.CreateGamePacket(0, ObjectDetachMessage(PlanetSideGUID(player.guid), item_guid, player.getPosition, 0, 0, 0)))
        sendResponse(PacketCoding.CreateGamePacket(0, DropItemMessage(item_guid)))
      }

    case msg @ PickupItemMessage(item_guid, player_guid, unk1, unk2) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, PickupItemMessage(item_guid, player_guid, unk1, unk2)))
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectAttachMessage(player_guid, item_guid, 250))) // item on mouse

    case msg @ ReloadMessage(item_guid, ammo_clip, unk1) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        var ammo_clip2 : Int = 0
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        if(player.getEquipmentInHolster(player.getUsedHolster).get.getName == "r_shotgun") ammo_clip2 = 16
        else if(player.getEquipmentInHolster(player.getUsedHolster).get.getName == "gauss") ammo_clip2 = 30
        else if(player.getEquipmentInHolster(player.getUsedHolster).get.getName == "mini_chaingun") ammo_clip2 = 100
        else if(player.getEquipmentInHolster(player.getUsedHolster).get.getName == "cycler") ammo_clip2 = 50
        else if(player.getEquipmentInHolster(player.getUsedHolster).get.getName == "lasher") ammo_clip2 = 35
        else if(player.getEquipmentInHolster(player.getUsedHolster).get.getName == "pulsar") ammo_clip2 = 40
        else if(player.getEquipmentInHolster(player.getUsedHolster).get.getName == "flechette") ammo_clip2 = 12
        else if(player.getEquipmentInHolster(player.getUsedHolster).get.getName == "rocklet") ammo_clip2 = 6
        else if(player.getEquipmentInHolster(player.getUsedHolster).get.getName == "bolt_driver") ammo_clip2 = 1
        else if(player.getEquipmentInHolster(player.getUsedHolster).get.getName == "flamethrower") ammo_clip2 = 100

        player.getEquipmentInHolster(player.getUsedHolster).get.magazine = player.getEquipmentInHolster(player.getUsedHolster).get.getFireMode.magazineSize
        player.lastShotSeq_time = -1

        sendResponse(PacketCoding.CreateGamePacket(0, ReloadMessage(item_guid, ammo_clip2, unk1)))
        avatarService ! AvatarService.ReloadMsg(item_guid, ammo_clip2, sessionId)
      }

    case msg@ObjectHeldMessage(avatar_guid, held_holsters, unk1) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(avatar_guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        if(held_holsters != 255) {
          player.setUsedHolster(held_holsters)
//          log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " getName: " + player.getEquipmentInHolster(player.getUsedHolster).get.getName)
//          log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " guid: " + player.getEquipmentInHolster(player.getUsedHolster).get.guid)
//          log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " toolDef: " + player.getEquipmentInHolster(player.getUsedHolster).get.toolDef)
//          log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " getAmmoType: " + player.getEquipmentInHolster(player.getUsedHolster).get.getAmmoType)
//          log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " getAmmoTypeIndex: " + player.getEquipmentInHolster(player.getUsedHolster).get.getAmmoTypeIndex)
//          log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " getUsedHolster1: " + player.getUsedHolster)
//          log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " getUsedHolster2: " + player.getHolster(player.getUsedHolster))
//          log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " getUsedHolster3: " + player.getEquipmentInHolster(player.getUsedHolster))
//          log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " isDefined: " + player.getHolster(player.getUsedHolster).getEquipment.isDefined)
        }
        avatarService ! AvatarService.ObjectHeld(PlanetSideGUID(player.guid))
      }

    case msg@AvatarJumpMessage(state) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        if (player.greenStamina - 10 <= 0) player.greenStamina = 0
        if (player.greenStamina - 10 > 0) player.greenStamina -= 10
        sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(PlanetSideGUID(player.guid), 2, player.greenStamina)))
      }

    case msg@ZipLineMessage(player_guid, origin_side, action, id, pos) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)
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
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)
      // TODO: Make sure this is the correct response in all cases
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(object_guid, 0)))

    case msg@ObjectDeleteMessage(object_guid, unk1) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(object_guid, 0)))

    case msg@MoveItemMessage(item_guid, avatar_guid_1, avatar_guid_2, dest, unk1) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)
      //sendResponse(PacketCoding.CreateGamePacket(0, ObjectAttachMessage(avatar_guid_1, item_guid, dest))) //for rexo test
//      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
//      if (playerOpt.isDefined) {
//        val player: PlayerAvatar = playerOpt.get
//
////        player.getEquipmentInHolster(player.getUsedHolster).get.toolDef
//
//        player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.rocklet))
//        player.inventory.resize(9,9)
//        player.inventory.addItem(Tool(item_guid.guid,ObjectClass.katana),6)
//        println(player.inventory.owner)
//        println(player.inventory)
//
//        val tool = player.inventory.getItem(item_guid.guid)
//        println(tool)
//        if(tool.isDefined) {
//          println(tool.get.guid)
//          println(tool.get.asInstanceOf[Tool].getToolDefinition.guid)
//          println(tool.get.asInstanceOf[Tool].getToolDefinition.name)
//
//
////          if (dest <= 3) {
////            player.setEquipmentInHolster(dest, Tool(item_guid.guid, 1))
//////            player.setEquipmentInHolster()
////          }
//        }
//
//      }
//////      Tool(item_guid.guid,100)
////
////      player.setUsedHolster(held_holsters)
////      log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " getName: " + player.getEquipmentInHolster(player.getUsedHolster).get.getName)
////      log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " guid: " + player.getEquipmentInHolster(player.getUsedHolster).get.guid)
////      log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " toolDef: " + player.getEquipmentInHolster(player.getUsedHolster).get.toolDef)
////      log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " getAmmoType: " + player.getEquipmentInHolster(player.getUsedHolster).get.getAmmoType)
////      log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " getAmmoTypeIndex: " + player.getEquipmentInHolster(player.getUsedHolster).get.getAmmoTypeIndex)
////      log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " getUsedHolster1: " + player.getUsedHolster)
////      log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " getUsedHolster2: " + player.getHolster(player.getUsedHolster))
////      log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " getUsedHolster3: " + player.getEquipmentInHolster(player.getUsedHolster))
////      log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " isDefined: " + player.getHolster(player.getUsedHolster).getEquipment.isDefined)
//


    case msg@ChangeAmmoMessage(item_guid, unk1) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        if (item_guid.guid == player.guid + 1 && player.getEquipmentInHolster(0).get.getAmmoTypeIndex == 0)  player.getEquipmentInHolster(0).get.setAmmoTypeIndex(1)
        else if (item_guid.guid == player.guid + 1 && player.getEquipmentInHolster(0).get.getAmmoTypeIndex == 1)  player.getEquipmentInHolster(0).get.setAmmoTypeIndex(0)
        else if (item_guid.guid == player.guid + 2 && player.getEquipmentInHolster(1).get.getAmmoTypeIndex == 0)  player.getEquipmentInHolster(1).get.setAmmoTypeIndex(1)
        else if (item_guid.guid == player.guid + 2 && player.getEquipmentInHolster(1).get.getAmmoTypeIndex == 1)  player.getEquipmentInHolster(1).get.setAmmoTypeIndex(0)
//        else if (item_guid.guid == player.guid + 5 && player.getEquipmentInHolster(2).get.getAmmoTypeIndex == 0)  player.getEquipmentInHolster(2).get.setAmmoTypeIndex(1)
//        else if (item_guid.guid == player.guid + 5 && player.getEquipmentInHolster(2).get.getAmmoTypeIndex == 1)  player.getEquipmentInHolster(2).get.setAmmoTypeIndex(0)
        else if (item_guid.guid == player.guid + 7 && player.getEquipmentInHolster(4).get.getAmmoTypeIndex == 0)  player.getEquipmentInHolster(4).get.setAmmoTypeIndex(1)
        else if (item_guid.guid == player.guid + 7 && player.getEquipmentInHolster(4).get.getAmmoTypeIndex == 1)  player.getEquipmentInHolster(4).get.setAmmoTypeIndex(0)
//        if (player.fav_Infantry_Loadout == 3 && player.weapon_ammo_mode == 0) {
//          sendResponse(PacketCoding.CreateGamePacket(0, ObjectDetachMessage(item_guid,PlanetSideGUID(player.guid + 6),Vector3(0.0f,0.0f,0.0f),0,0,0)))
//          Thread.sleep(200)
//          sendResponse(PacketCoding.CreateGamePacket(0, ObjectAttachMessage(item_guid, PlanetSideGUID(player.guid + 6), 0)))
//          Thread.sleep(200)
//          sendResponse(PacketCoding.CreateGamePacket(0, ChangeAmmoMessage(item_guid, 6)))
//          player.weapon_ammo_mode = 1
//        }
//        else if (player.fav_Infantry_Loadout == 3 && player.weapon_ammo_mode == 1) {
//          sendResponse(PacketCoding.CreateGamePacket(0, ObjectDetachMessage(item_guid,PlanetSideGUID(player.guid + 6),Vector3(0.0f,0.0f,0.0f),0,0,0)))
//          Thread.sleep(200)
//          sendResponse(PacketCoding.CreateGamePacket(0, ObjectAttachMessage(item_guid, PlanetSideGUID(player.guid + 6), 0)))
//          Thread.sleep(200)
//          sendResponse(PacketCoding.CreateGamePacket(0, ChangeAmmoMessage(item_guid, 6)))
//          player.weapon_ammo_mode = 0
//        }
//        if (player.fav_Infantry_Loadout == 0 && player.weapon_ammo_mode == 0) {
//          sendResponse(PacketCoding.CreateGamePacket(0, ObjectDetachMessage(item_guid,PlanetSideGUID(player.guid + 6),Vector3(0.0f,0.0f,0.0f),0,0,0)))
//          Thread.sleep(200)
//          sendResponse(PacketCoding.CreateGamePacket(0, ObjectAttachMessage(item_guid, PlanetSideGUID(player.guid + 6), 0)))
//          Thread.sleep(200)
//          sendResponse(PacketCoding.CreateGamePacket(0, ChangeAmmoMessage(item_guid, 100)))
//          player.weapon_ammo_mode = 1
//        }
//        else if (player.fav_Infantry_Loadout == 0 && player.weapon_ammo_mode == 1) {
//          sendResponse(PacketCoding.CreateGamePacket(0, ObjectDetachMessage(item_guid,PlanetSideGUID(player.guid + 6),Vector3(0.0f,0.0f,0.0f),0,0,0)))
//          Thread.sleep(200)
//          sendResponse(PacketCoding.CreateGamePacket(0, ObjectAttachMessage(item_guid, PlanetSideGUID(player.guid + 6), 0)))
//          Thread.sleep(200)
//          sendResponse(PacketCoding.CreateGamePacket(0, ChangeAmmoMessage(item_guid, 100)))
//          player.weapon_ammo_mode = 0
//        }
//        avatarService ! AvatarService.ChangeAmmoMode(item_guid, sessionId)
      }

    case msg@UseItemMessage(avatar_guid, unk1, object_guid, unk2, unk3, unk4, unk5, unk6, unk7, unk8, itemType) =>
      // TODO: Not all fields in the response are identical to source in real packet logs (but seems to be ok)
      // TODO: Not all incoming UseItemMessage's respond with another UseItemMessage (i.e. doors only send out GenericObjectStateMsg)
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(avatar_guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        if (itemType != 121 && unk1 - player.guid != 3) sendResponse(PacketCoding.CreateGamePacket(0, UseItemMessage(avatar_guid, unk1, object_guid, unk2, unk3, unk4, unk5, unk6, unk7, unk8, itemType)))
        if (itemType == 121 && unk3 && player.getEquipmentInHolster(player.getUsedHolster).get.getName == "bank") {
          // TODO : bank ?
          val OnlinePlayer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(object_guid)
          if (OnlinePlayer.isDefined) {
            val onlineplayer: PlayerAvatar = OnlinePlayer.get
            if (player.guid != onlineplayer.guid && player.vel.isEmpty && distance(player.getPosition, onlineplayer.getPosition) < 5 && player.faction == onlineplayer.faction) {
              if (onlineplayer.getMaxPersonalArmor - onlineplayer.blueArmor <= 15) {
                onlineplayer.blueArmor = onlineplayer.getMaxPersonalArmor
                //                sendResponse(PacketCoding.CreateGamePacket(0, QuantityUpdateMessage(PlanetSideGUID(8214),ammo_quantity_left)))
                val RepairPercent: Int = onlineplayer.blueArmor * 100 / onlineplayer.getMaxPersonalArmor
                sendResponse(PacketCoding.CreateGamePacket(0, RepairMessage(object_guid, RepairPercent)))
                avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(onlineplayer.guid), 4, onlineplayer.blueArmor)
              }
              if (onlineplayer.getMaxPersonalArmor - onlineplayer.blueArmor > 15) {
                onlineplayer.blueArmor += 15
                //                sendResponse(PacketCoding.CreateGamePacket(0, QuantityUpdateMessage(PlanetSideGUID(8214),ammo_quantity_left)))
                val RepairPercent: Int = onlineplayer.blueArmor * 100 / onlineplayer.getMaxPersonalArmor
                sendResponse(PacketCoding.CreateGamePacket(0, RepairMessage(object_guid, RepairPercent)))
                avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(onlineplayer.guid), 4, onlineplayer.blueArmor)
              }
            }
          }
          if (PlanetSideGUID(player.guid) == object_guid && player.vel.isEmpty) {
            if (player.getMaxPersonalArmor - player.blueArmor <= 15) {
              player.blueArmor = player.getMaxPersonalArmor
              //              sendResponse(PacketCoding.CreateGamePacket(0, QuantityUpdateMessage(PlanetSideGUID(8214),ammo_quantity_left)))
              sendResponse(PacketCoding.CreateGamePacket(0, RepairMessage(object_guid, player.blueArmor)))
              avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid), 4, player.blueArmor)
            }
            if (player.getMaxPersonalArmor - player.blueArmor > 15) {
              player.blueArmor += 15
              //              sendResponse(PacketCoding.CreateGamePacket(0, QuantityUpdateMessage(PlanetSideGUID(8214),ammo_quantity_left)))
              sendResponse(PacketCoding.CreateGamePacket(0, RepairMessage(object_guid, player.blueArmor)))
              avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid), 4, player.blueArmor)
            }
          }
        }
        if (itemType == 121 && unk3 && player.getEquipmentInHolster(player.getUsedHolster).get.getName == "medicalapplicator") {
          // TODO : med app ?
          val OnlinePlayer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(object_guid)
          if (OnlinePlayer.isDefined) {
            val onlineplayer: PlayerAvatar = OnlinePlayer.get
            if (player.guid != onlineplayer.guid && player.vel.isEmpty && distance(player.getPosition, onlineplayer.getPosition) < 5 && player.faction == onlineplayer.faction && onlineplayer.death_by == 0) {
              if (onlineplayer.getMaxHealth - onlineplayer.redHealth <= 5) {
                onlineplayer.redHealth = onlineplayer.getMaxHealth
                //                sendResponse(PacketCoding.CreateGamePacket(0, QuantityUpdateMessage(PlanetSideGUID(8214),ammo_quantity_left)))
                val RepairPercent: Int = onlineplayer.redHealth * 100 / onlineplayer.getMaxHealth
                sendResponse(PacketCoding.CreateGamePacket(0, RepairMessage(object_guid, RepairPercent)))
                avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(onlineplayer.guid), 0, onlineplayer.redHealth)
              }
              if (onlineplayer.getMaxHealth - onlineplayer.redHealth > 5) {
                onlineplayer.redHealth += 5
                //                sendResponse(PacketCoding.CreateGamePacket(0, QuantityUpdateMessage(PlanetSideGUID(8214),ammo_quantity_left)))
                val RepairPercent: Int = onlineplayer.redHealth * 100 / onlineplayer.getMaxHealth
                sendResponse(PacketCoding.CreateGamePacket(0, RepairMessage(object_guid, RepairPercent)))
                avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(onlineplayer.guid), 0, onlineplayer.redHealth)
              }
            }
          }
          if (PlanetSideGUID(player.guid) == object_guid && player.vel.isEmpty) {
            if (player.getMaxHealth - player.redHealth <= 5) {
              player.redHealth = player.getMaxHealth
              //              sendResponse(PacketCoding.CreateGamePacket(0, QuantityUpdateMessage(PlanetSideGUID(8214),ammo_quantity_left)))
              sendResponse(PacketCoding.CreateGamePacket(0, RepairMessage(object_guid, player.redHealth)))
              avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid), 0, player.redHealth)
            }
            if (player.getMaxHealth - player.redHealth > 5) {
              player.redHealth += 5
              //              sendResponse(PacketCoding.CreateGamePacket(0, QuantityUpdateMessage(PlanetSideGUID(8214),ammo_quantity_left)))
              sendResponse(PacketCoding.CreateGamePacket(0, RepairMessage(object_guid, player.redHealth)))
              avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid), 0, player.redHealth)
            }
          }
        }

        if (itemType == 121 && !unk3) {
          // TODO : medkit use ?!
          //        if (playerOpt.isDefined) {
          //          val player: PlayerAvatar = playerOpt.get
          if (player.getMaxHealth - player.redHealth == 0) {
            sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_225, false, "", "@HealComplete", None)))
          }
          else if (System.currentTimeMillis() - player.lastMedkit < 5000) {
            sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.UNK_225, false, "", "@TimeUntilNextUse^"+(5 - (System.currentTimeMillis() - player.lastMedkit) / 1000).toString+"~", None)))
          }
          else if (player.getMaxHealth - player.redHealth <= 25 && player.getMaxHealth - player.redHealth != 0 && System.currentTimeMillis() - player.lastMedkit > 5000) {
            player.redHealth = player.getMaxHealth
//                        sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(avatar_guid, 0, player.redHealth)))
            avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid), 0, player.redHealth)
            sendResponse(PacketCoding.CreateGamePacket(0, UseItemMessage(avatar_guid, unk1, object_guid, 0, unk3, unk4, unk5, unk6, unk7, unk8, itemType)))
            //            sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(unk1), 2))) // for nexus fight
            sendResponse(PacketCoding.CreateGamePacket(0, AvatarVehicleTimerMessage(avatar_guid,"medkit",5,false)))
            player.lastMedkit = System.currentTimeMillis()
          }
          else if (player.getMaxHealth - player.redHealth > 25 && System.currentTimeMillis() - player.lastMedkit > 5000) {
            player.redHealth += 25
            //            sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(avatar_guid, 0, player.redHealth)))
            avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid), 0, player.redHealth)
            sendResponse(PacketCoding.CreateGamePacket(0, UseItemMessage(avatar_guid, unk1, object_guid, 0, unk3, unk4, unk5, unk6, unk7, unk8, itemType)))
            //            sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(unk1), 2))) // for nexus fight
            sendResponse(PacketCoding.CreateGamePacket(0, AvatarVehicleTimerMessage(avatar_guid,"medkit",5,false)))
            player.lastMedkit = System.currentTimeMillis()
          }
        }
        //      }
        if (unk1 == 0 && !unk3 && unk7 == 25) {
          // TODO: This should only actually be sent to doors upon opening; may break non-door items upon use
          if (player.continent != "i2") {
            avatarService ! AvatarService.Doors(PlanetSideGUID(player.guid), object_guid.guid)
          }
          if (player.continent == "i2") {

            if(object_guid.guid != 274 || object_guid.guid != 276 || object_guid.guid != 284 || object_guid.guid != 231 || object_guid.guid != 233 || object_guid.guid != 235) {
              avatarService ! AvatarService.Doors(PlanetSideGUID(player.guid), object_guid.guid)
            }
            if (object_guid.guid == 274 || object_guid.guid == 276 || object_guid.guid == 284) { // i2 NC spawn
//              sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(1714, 1749, 91),0)))) // rashnu
              sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(1761, 2645, 85),0)))) // zal
            }
//            if (object_guid.guid == 247 || object_guid.guid == 248 || object_guid.guid == 256) { // i2 TR spawn
//              sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(1806, 1671, 83),0)))) // rashnu
//            }
            if (object_guid.guid == 231 || object_guid.guid == 233 || object_guid.guid == 235) { // i2 VS spawn
              sendResponse(PacketCoding.CreateGamePacket(0, PlayerStateShiftMessage(ShiftState(0,Vector3(1831, 2689, 91),0)))) // zal
            }
          }
        }
      }

    case msg @ UnuseItemMessage(player, item) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg@GenericObjectStateMsg(object_guid, unk1) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg @ DeployObjectMessage(guid, unk1, pos, roll, pitch, yaw, unk2) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg@ItemTransactionMessage(terminal_guid, transaction_type, item_page, item_name, unk1, item_guid) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)

        if (transaction_type == TransactionType.Sell) {
          sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(item_guid, 0)))
          sendResponse(PacketCoding.CreateGamePacket(0, ItemTransactionResultMessage(terminal_guid, transaction_type, true)))
        }
        if (transaction_type == TransactionType.Buy) {
          // buy item on equip term
          /*          val obj = AmmoBoxData(50)
          //          val msg = ObjectCreateMessage(0, 28, PlanetSideGUID(1280), ObjectCreateMessageParent(PlanetSideGUID(player.guid), 250), obj)
          //          val pkt = PacketCoding.EncodePacket(msg).require.toByteVector
                    sendRawResponse(pkt)*/
          if(item_name == "katana") {
            var color : Int = 0 // TR
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(player.guid + 7), 0)))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(player.guid + 8), 0)))
            Thread.sleep(100)
            if (player.faction == PlanetSideEmpire.NC) color = 4
            if (player.faction == PlanetSideEmpire.VS) color = 8
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0, ObjectClass.katana, PlanetSideGUID(player.guid + 7), Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid), 4)),
              Some(DetailedConcurrentFeedWeaponData(color, 8,
                List(InternalSlot(ObjectClass.melee_ammo, PlanetSideGUID(player.guid + 8), 0, DetailedAmmoBoxData(0,1)),InternalSlot(ObjectClass.melee_ammo, PlanetSideGUID(player.guid + 23), 1, DetailedAmmoBoxData(0,1))))))))
            player.setEquipmentInHolster(4, Tool(player.guid + 7, ObjectClass.katana))
            avatarService ! AvatarService.ChangeWeapon(4, sessionId)
          }
        }
        if (transaction_type == TransactionType.Infantry_Loadout) {
          player.lastShotSeq_time = -1
          // clean the old weapon
          for (i : Int <- 1 to 23) {
            if (i == 10) Thread.sleep(100)
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(player.guid + i), 0)))
          }
          Thread.sleep(50)
          // change armor
          if(unk1 <= 4) player.setExoSuitType(ExoSuitType.Reinforced)
          else if (unk1 >= 5) player.setExoSuitType(ExoSuitType.Agile)
          sendResponse(PacketCoding.CreateGamePacket(0, ArmorChangedMessage(PlanetSideGUID(player.guid),player.getExoSuitType,0)))
          Thread.sleep(50)

          // load weapons & tools
          // tools
          player.setEquipmentInHolster(0, Tool(player.guid + 1, ObjectClass.bank))
          player.setEquipmentInHolster(1, Tool(player.guid + 3, ObjectClass.medicalapplicator))
          // knives
          if (player.faction == PlanetSideEmpire.NC) player.setEquipmentInHolster(4, Tool(player.guid + 7, ObjectClass.magcutter)) // magcutter
          else if (player.faction == PlanetSideEmpire.TR) player.setEquipmentInHolster(4, Tool(player.guid + 7, ObjectClass.chainblade)) // chainblade
          else if (player.faction == PlanetSideEmpire.VS) player.setEquipmentInHolster(4, Tool(player.guid + 7, ObjectClass.forceblade)) // forceblade

          if(unk1 == 0) { //define fav 1
            if (player.faction == PlanetSideEmpire.NC) { // JH
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.r_shotgun))
            }
            else if (player.faction == PlanetSideEmpire.TR) { // MCG
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.mini_chaingun))
            }
            else if (player.faction == PlanetSideEmpire.VS) { // Lasher
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.lasher))
            }
            player.setEquipmentInHolster(3, Tool(player.guid + 9, ObjectClass.rocklet))
          }
          if(unk1 == 1) { //define fav 2
            if (player.faction == PlanetSideEmpire.NC) { // JH + Gauss
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.r_shotgun))
              player.setEquipmentInHolster(3, Tool(player.guid + 9, ObjectClass.gauss))
            }
            else if (player.faction == PlanetSideEmpire.TR) { // MCG + Cycler
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.mini_chaingun))
              player.setEquipmentInHolster(3, Tool(player.guid + 9, ObjectClass.cycler))
            }
            else if (player.faction == PlanetSideEmpire.VS) { // Lasher + Pulsar
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.lasher))
              player.setEquipmentInHolster(3, Tool(player.guid + 9, ObjectClass.pulsar))
            }
          }
          if(unk1 == 2) { //define fav 3
            if (player.faction == PlanetSideEmpire.NC) { // Gauss
              player.setEquipmentInHolster(3, Tool(player.guid + 9, ObjectClass.gauss))
            }
            else if (player.faction == PlanetSideEmpire.TR) { // Cycler
              player.setEquipmentInHolster(3, Tool(player.guid + 9, ObjectClass.cycler))
            }
            else if (player.faction == PlanetSideEmpire.VS) { // Pulsar
              player.setEquipmentInHolster(3, Tool(player.guid + 9, ObjectClass.pulsar))
            }
            player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.flechette)) // Sweeper
          }
          if(unk1 == 3) { //define fav 4
            if (player.faction == PlanetSideEmpire.NC) { // JH
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.r_shotgun))
            }
            else if (player.faction == PlanetSideEmpire.TR) { // MCG
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.mini_chaingun))
            }
            else if (player.faction == PlanetSideEmpire.VS) { // Lasher
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.lasher))
            }
            player.setEquipmentInHolster(3, Tool(player.guid + 9, ObjectClass.flamethrower))
          }
          if(unk1 == 4) { //define fav 5
            if (player.faction == PlanetSideEmpire.NC) { // JH
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.gauss))
            }
            else if (player.faction == PlanetSideEmpire.TR) { // MCG
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.cycler))
            }
            else if (player.faction == PlanetSideEmpire.VS) { // Lasher
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.pulsar))
            }
            player.setEquipmentInHolster(3, Tool(player.guid + 9, ObjectClass.bolt_driver))
          }
          if(unk1 == 5) { //define fav 6
            if (player.faction == PlanetSideEmpire.NC) { // JH
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.r_shotgun))
            }
            else if (player.faction == PlanetSideEmpire.TR) { // MCG
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.mini_chaingun))
            }
            else if (player.faction == PlanetSideEmpire.VS) { // Lasher
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.lasher))
            }
          }
          if(unk1 == 6) { //define fav 7
            player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.flechette))
          }
          if(unk1 == 7) { //define fav 8
            if (player.faction == PlanetSideEmpire.NC) { // Gauss
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.gauss))
            }
            else if (player.faction == PlanetSideEmpire.TR) { // Cycler
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.cycler))
            }
            else if (player.faction == PlanetSideEmpire.VS) { // Pulsar
              player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.pulsar))
            }
          }
          if(unk1 == 8) { //define fav 9
            player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.bolt_driver))
          }
          if(unk1 == 9) { //define fav 10
            player.setEquipmentInHolster(2, Tool(player.guid + 5, ObjectClass.rocklet))
          }

          // load weapons
          for (ind <- 0 to 4) {
            if (player.getHolster(ind).getEquipment.isDefined) {
              if (!player.getEquipmentInHolster(ind).get.getToolDefinition.isConcurrentFeed){
                if (player.getEquipmentInHolster(ind).get.getName == "r_shotgun" || player.getEquipmentInHolster(ind).get.getName == "flechette") {
                  sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0,
                    player.getEquipmentInHolster(ind).get.toolDef, PlanetSideGUID(player.getEquipmentInHolster(ind).get.guid),
                    Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid), ind)),
                    Some(DetailedWeaponData(12,
                      InternalSlot(player.getEquipmentInHolster(ind).get.getAmmoType.id, PlanetSideGUID(player.getEquipmentInHolster(ind).get.guid + 1), 0,
                        DetailedAmmoBoxData(0, player.getEquipmentInHolster(ind).get.getFireMode.magazineSize/8)))))))
                }
                else {
                  sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0,
                    player.getEquipmentInHolster(ind).get.toolDef, PlanetSideGUID(player.getEquipmentInHolster(ind).get.guid),
                    Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid), ind)),
                    Some(DetailedWeaponData(12,
                      InternalSlot(player.getEquipmentInHolster(ind).get.getAmmoType.id, PlanetSideGUID(player.getEquipmentInHolster(ind).get.guid + 1), 0,
                        DetailedAmmoBoxData(0, player.getEquipmentInHolster(ind).get.getFireMode.magazineSize)))))))
                }
              }
              else {
                var color : Int = 0 // TR
                if (player.faction == PlanetSideEmpire.NC) color = 4
                if (player.faction == PlanetSideEmpire.VS) color = 8

                sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0,
                  player.getEquipmentInHolster(ind).get.toolDef, PlanetSideGUID(player.getEquipmentInHolster(ind).get.guid),
                  Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid), ind)),
                  Some(DetailedConcurrentFeedWeaponData(color, 8,
                    List(InternalSlot(player.getEquipmentInHolster(ind).get.getAmmoType.id, PlanetSideGUID(player.getEquipmentInHolster(ind).get.guid + 1), 0,
                      DetailedAmmoBoxData(0, player.getEquipmentInHolster(ind).get.getFireMode.magazineSize)),
                      InternalSlot(player.getEquipmentInHolster(ind).get.getAmmoType.id, PlanetSideGUID(player.getEquipmentInHolster(ind).get.guid + 16), 1,
                        DetailedAmmoBoxData(0, player.getEquipmentInHolster(ind).get.getFireMode.magazineSize))))))))
              }
              player.getEquipmentInHolster(ind).get.magazine = player.getEquipmentInHolster(ind).get.getFireMode.magazineSize
            }
          }

          // change inventory box
          if(unk1 <= 4) {
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.shotgun_shell,PlanetSideGUID(player.guid + 18),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),6)),Some(DetailedAmmoBoxData(8,16)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.bullet_9mm,PlanetSideGUID(player.guid + 11),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),9)),Some(DetailedAmmoBoxData(8,100)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.energy_cell,PlanetSideGUID(player.guid + 19),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),12)),Some(DetailedAmmoBoxData(8,50)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.rocket,PlanetSideGUID(player.guid + 12),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),15)),Some(DetailedAmmoBoxData(8,24)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.frag_cartridge,PlanetSideGUID(player.guid + 13),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),42)),Some(DetailedAmmoBoxData(8,24)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.bolt,PlanetSideGUID(player.guid + 14),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),51)),Some(DetailedAmmoBoxData(8,10)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.bullet_9mm_AP,PlanetSideGUID(player.guid + 15),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),78)),Some(DetailedAmmoBoxData(8,100)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.medkit,PlanetSideGUID(player.guid + 17),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),98)),Some(DetailedAmmoBoxData(8,1)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.flamethrower_ammo,PlanetSideGUID(player.guid + 20),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),45)),Some(DetailedAmmoBoxData(8,100)))))
          }
          else if (unk1 >= 5) {
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.shotgun_shell,PlanetSideGUID(player.guid + 18),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),9)),Some(DetailedAmmoBoxData(8,16)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.bullet_9mm,PlanetSideGUID(player.guid + 11),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),6)),Some(DetailedAmmoBoxData(8,100)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.energy_cell,PlanetSideGUID(player.guid + 19),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),12)),Some(DetailedAmmoBoxData(8,50)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.rocket,PlanetSideGUID(player.guid + 12),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),33)),Some(DetailedAmmoBoxData(8,24)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.frag_cartridge,PlanetSideGUID(player.guid + 13),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),36)),Some(DetailedAmmoBoxData(8,24)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.bolt,PlanetSideGUID(player.guid + 14),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),39)),Some(DetailedAmmoBoxData(8,10)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.bullet_9mm_AP,PlanetSideGUID(player.guid + 15),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),60)),Some(DetailedAmmoBoxData(8,100)))))
            sendResponse(PacketCoding.CreateGamePacket(0, ObjectCreateDetailedMessage(0 ,ObjectClass.medkit,PlanetSideGUID(player.guid + 17),
              Some(ObjectCreateMessageParent(PlanetSideGUID(player.guid),73)),Some(DetailedAmmoBoxData(8,1)))))
          }

          sendResponse(PacketCoding.CreateGamePacket(0, ItemTransactionResultMessage(terminal_guid,TransactionType.Infantry_Loadout,true,0)))

          avatarService ! AvatarService.ChangeWeapon(unk1 + 10, sessionId)
        }
      }

    case msg@WeaponDelayFireMessage(seq_time, weapon_guid) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg@WeaponFireMessage(seq_time, weapon_guid, projectile_guid, shot_origin, unk1, unk2, unk3, unk4, unk5, unk6, unk7) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        player.getEquipmentInHolster(player.getUsedHolster).get.magazine -= 1
        if (player.lastShotSeq_time != -1) {
          var time : Int = 0
          if (seq_time - player.lastShotSeq_time < 0) {
            time = 1024 + (seq_time - player.lastShotSeq_time)}
          else time = seq_time - player.lastShotSeq_time
          if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "gauss" && time < 1) {
            discordROF(PlanetSideGUID(player.guid),player.name)
          }
          if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "r_shotgun" && player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 0 && time > 0 && time < 1) {
            discordROF(PlanetSideGUID(player.guid),player.name)
          }
          if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "r_shotgun" && player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 1 && time > 0 && time < 1) {
            discordROF(PlanetSideGUID(player.guid),player.name)
          }
          if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "flechette" && time > 0 && time < 1) {
            discordROF(PlanetSideGUID(player.guid),player.name)
          }
          if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "rocklet" && player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 0 && time < 1) {
            discordROF(PlanetSideGUID(player.guid),player.name)
          }
          if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "rocklet" && player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 1 && time < 1) {
            discordROF(PlanetSideGUID(player.guid),player.name)
          }
          if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "flamethrower" && player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 0 && time < 1) {
            discordROF(PlanetSideGUID(player.guid),player.name)
          }
          if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "flamethrower" && player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 1 && time < 1) {
            discordROF(PlanetSideGUID(player.guid),player.name)
          }
          if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "mini_chaingun" && time < 1) {
            discordROF(PlanetSideGUID(player.guid),player.name)
          }
          if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "cycler" && time < 1) {
            discordROF(PlanetSideGUID(player.guid),player.name)
          }
          if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "lasher" && time < 1) {
            discordROF(PlanetSideGUID(player.guid),player.name)
          }
          if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "pulsar" && time < 1) {
            discordROF(PlanetSideGUID(player.guid),player.name)
          }
        }
        player.lastShotSeq_time = seq_time
      }

    case msg@WeaponDryFireMessage(weapon_guid) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg @ TargetingImplantRequest(list) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg@WeaponLazeTargetPositionMessage(weapon, pos1, pos2) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " Lazing position: " + pos2.toString)

    case msg@HitMessage(seq_time, projectile_guid, unk1, hit_info, unk2, unk3, unk4) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        if (hit_info.isDefined) {
          if (hit_info.get.hitobject_guid.isDefined) {
            val OnlinePlayer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(hit_info.get.hitobject_guid.get.guid)
            if (OnlinePlayer.isDefined && !player.spectator) {
              val onlineplayer: PlayerAvatar = OnlinePlayer.get
              if (distance(hit_info.get.hit_pos, onlineplayer.getPosition) >= 6) {
                discordPullH(PlanetSideGUID(player.guid), player.name)
              }
            }
            else if (OnlinePlayer.isEmpty) {
              if (hit_info.get.hitobject_guid.get.guid >= 15000) {
                var uid : Int = 15000
                while (uid != 60000) {
                  if (hit_info.get.hitobject_guid.get.guid != uid || uid == 40100) {
                    uid += 100
                  }
                  else if (hit_info.get.hitobject_guid.get.guid == uid) {
                    for (id <- 1 to 23) {
                      sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(hit_info.get.hitobject_guid.get.guid + id),4)))
                    }
                    sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(hit_info.get.hitobject_guid.get.guid),4)))
                    uid = 60000
                  }
                }
              }
            }
          }
        }
      }


    case msg@SplashHitMessage(seq_time, projectile_uid, explosion_pos, direct_victim_uid, unk3, projectile_vel, unk4, targets) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        val OnlinePlayer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(direct_victim_uid)
        if (OnlinePlayer.isDefined && !player.spectator && player.getUsedHolster != 255) {
          val onlineplayer: PlayerAvatar = OnlinePlayer.get

          if (distance(explosion_pos, onlineplayer.getPosition) >= 6) {
            discordPullH(PlanetSideGUID(player.guid), player.name)
          }

          if ( !onlineplayer.spectator && onlineplayer.continent == "i2") {

            avatarService ! AvatarService.HitHintReturn(PlanetSideGUID(player.guid), PlanetSideGUID(onlineplayer.guid))
            val distanceBetweenPlayers: Float = distance(player.getPosition, onlineplayer.getPosition)
            var currentDamage: Int = 0
            var currentResistance: Int = 0


            if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "rocklet" && player.getEquipmentInHolster(player.getUsedHolster).get.getAmmoTypeIndex == 0) {
              // Rocklet
              currentDamage = damages(rocket_projectile_velocity, rocket_projectile_lifespan, rocket_projectile_lifespan, 0.0f, rocket_projectile_damage, distanceBetweenPlayers)
            }
            if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "bolt_driver") {
              // Bolt Driver
              currentDamage = damages(bolt_projectile_velocity, bolt_projectile_lifespan, bolt_projectile_lifespan, 0.0f, bolt_projectile_damage0, distanceBetweenPlayers)
            }

            if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "flamethrower" && player.getEquipmentInHolster(player.getUsedHolster).get.getFireModeIndex == 0) {
              // Dragon mode 1
              currentDamage = damages(flamethrower_projectile_velocity, flamethrower_projectile_lifespan, flamethrower_projectile_degrade_delay, flamethrower_projectile_degrade_multiplier, flamethrower_projectile_damage0, distanceBetweenPlayers)
            }
            else if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "flamethrower" && player.getEquipmentInHolster(player.getUsedHolster).get.getFireModeIndex == 1) {
              // Dragon mode 2
              currentDamage = damages(flamethrower_fireball_velocity, flamethrower_fireball_lifespan, flamethrower_fireball_lifespan, 0.0f, flamethrower_fireball_damage0, distanceBetweenPlayers)
            }

            if (onlineplayer.getExoSuitType == ExoSuitType.Agile && player.getEquipmentInHolster(player.getUsedHolster).get.getName != "flamethrower") {
              currentResistance = lite_armor_resistance_splash
            }
            //          else if (onlineplayer.getExoSuitType == ExoSuitType.Agile && player.getEquipmentInHolster(player.getUsedHolster).get.getName == "flamethrower") {
            //            currentResistance = lite_armor_resistance_aggravated
            //          }
            else if (onlineplayer.getExoSuitType == ExoSuitType.Reinforced && player.getEquipmentInHolster(player.getUsedHolster).get.getName != "flamethrower") {
              currentResistance = med_armor_resistance_splash
            }
            //          else if (onlineplayer.getExoSuitType == ExoSuitType.Reinforced && player.getEquipmentInHolster(player.getUsedHolster).get.getName == "flamethrower") {
            //            currentResistance = med_armor_resistance_aggravated
            //          }
            onlineplayer.redHealth = damagesAfterResist(currentDamage, currentResistance, onlineplayer.redHealth, onlineplayer.blueArmor)._1
            onlineplayer.blueArmor = damagesAfterResist(currentDamage, currentResistance, onlineplayer.redHealth, onlineplayer.blueArmor)._2
            avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(onlineplayer.guid), 0, onlineplayer.redHealth)
            avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(onlineplayer.guid), 4, onlineplayer.blueArmor)
            if (onlineplayer.redHealth == 0) {
              onlineplayer.death_by = player.guid
            }

          }
        }
        else if (OnlinePlayer.isEmpty) {
          if (direct_victim_uid.guid >= 15000) {
            var uid : Int = 15000
            while (uid != 60000) {
              if (direct_victim_uid.guid != uid || uid == 40100) {
                uid += 100
              }
              else if (direct_victim_uid.guid == uid) {
                for (id <- 1 to 23) {
                  sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(direct_victim_uid.guid + id),4)))
                }
                sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(direct_victim_uid.guid),4)))
                uid = 60000
              }
            }
          }
        }
        for(elem <- targets) {
          val OnlinePlayer2: Option[PlayerAvatar] = PlayerMasterList.getPlayer(elem.uid)
          if (OnlinePlayer2.isDefined && !player.spectator) {
            val onlineplayer2: PlayerAvatar = OnlinePlayer2.get

            if (distance(elem.pos, onlineplayer2.getPosition) >= 6) {
              discordPullH(PlanetSideGUID(player.guid), player.name)
            }

            if ( !onlineplayer2.spectator && onlineplayer2.continent == "i2") {

              avatarService ! AvatarService.HitHintReturn(PlanetSideGUID(player.guid), PlanetSideGUID(onlineplayer2.guid))
              val distanceBetweenPlayers: Float = distance(player.getPosition, onlineplayer2.getPosition)
              var currentDamage: Int = 0
              var currentResistance: Int = 0


              if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "rocklet" && player.getEquipmentInHolster(player.getUsedHolster).get.getAmmoTypeIndex == 0) {
                // Rocklet
                currentDamage = damages(rocket_projectile_velocity, rocket_projectile_lifespan, rocket_projectile_lifespan, 0.0f, rocket_projectile_damage, distanceBetweenPlayers)
              }
              if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "flamethrower") {
                // Dragon
                currentDamage = damages(flamethrower_fireball_velocity, flamethrower_fireball_lifespan, flamethrower_fireball_lifespan, 0.0f, flamethrower_fireball_damage0, distanceBetweenPlayers)
              }

              if (onlineplayer2.getExoSuitType == ExoSuitType.Agile && player.getEquipmentInHolster(player.getUsedHolster).get.getName != "flamethrower") {
                currentResistance = lite_armor_resistance_splash
              }
              //            else if (onlineplayer2.getExoSuitType == ExoSuitType.Agile && player.getEquipmentInHolster(player.getUsedHolster).get.getName == "flamethrower") {
              //              currentResistance = lite_armor_resistance_aggravated
              //            }
              else if (onlineplayer2.getExoSuitType == ExoSuitType.Reinforced && player.getEquipmentInHolster(player.getUsedHolster).get.getName != "flamethrower") {
                currentResistance = med_armor_resistance_splash
              }
              //            else if (onlineplayer2.getExoSuitType == ExoSuitType.Reinforced && player.getEquipmentInHolster(player.getUsedHolster).get.getName == "flamethrower") {
              //              currentResistance = med_armor_resistance_aggravated
              //            }
              onlineplayer2.redHealth = damagesAfterResist((currentDamage * 0.5).toInt, currentResistance, onlineplayer2.redHealth, onlineplayer2.blueArmor)._1
              onlineplayer2.blueArmor = damagesAfterResist((currentDamage * 0.5).toInt, currentResistance, onlineplayer2.redHealth, onlineplayer2.blueArmor)._2
              avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(onlineplayer2.guid), 0, onlineplayer2.redHealth)
              avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(onlineplayer2.guid), 4, onlineplayer2.blueArmor)
              if (onlineplayer2.redHealth == 0) {
                onlineplayer2.death_by = player.guid
              }
            }

            //rocket edge 50%
            //frag_cartridge
          }
          else if (OnlinePlayer2.isEmpty) {
            if (elem.uid.guid >= 15000) {
              var uid : Int = 15000
              while (uid != 60000) {
                if (elem.uid.guid != uid || uid == 40100) {
                  uid += 100
                }
                else if (elem.uid.guid == uid) {
                  for (id <- 1 to 23) {
                    sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(elem.uid.guid + id),4)))
                  }
                  sendResponse(PacketCoding.CreateGamePacket(0, ObjectDeleteMessage(PlanetSideGUID(elem.uid.guid),4)))
                  uid = 60000
                }
              }
            }
          }
        }
      }

    case msg @ DeployRequestMessage(player, entity, unk1, unk2, unk3, pos) =>
      //if you try to deploy, can not undeploy
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg@AvatarFirstTimeEventMessage(avatar_guid, object_guid, unk1, event_name) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg@AvatarGrenadeStateMessage(player_guid, state) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg@GenericActionMessage(action) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg@WarpgateRequest(continent_guid, building_guid, dest_building_guid, dest_continent_guid, unk1, unk2) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg@ProximityTerminalUseMessage(player_guid, object_guid, unk) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(player_guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        if(!unk && player.getVelocity.isEmpty){
          useProximityTerminalID = Option.apply(object_guid)
          sendResponse(PacketCoding.CreateGamePacket(0, ProximityTerminalUseMessage(PlanetSideGUID(player.guid), object_guid, true)))
          if (player.redHealth + 10 > player.getMaxHealth) player.redHealth = player.getMaxHealth
          if (player.redHealth + 10 <= player.getMaxHealth) player.redHealth += 10
          if (player.blueArmor + 10 > player.getMaxPersonalArmor) player.blueArmor = player.getMaxPersonalArmor
          if (player.blueArmor + 10 <= player.getMaxPersonalArmor) player.blueArmor += 10
          avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid),0,player.redHealth)
          avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid),4,player.blueArmor)
          if (player.redHealth == player.getMaxHealth && player.blueArmor == player.getMaxPersonalArmor) {
            sendResponse(PacketCoding.CreateGamePacket(0, ProximityTerminalUseMessage(PlanetSideGUID(0), object_guid, false)))
          }
        }
      }

    case msg@MountVehicleMsg(player_guid, vehicle_guid, entry_point) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(vehicle_guid, 0, 1000)))
      sendResponse(PacketCoding.CreateGamePacket(0, ObjectAttachMessage(vehicle_guid, player_guid, 0)))

    case msg@DismountVehicleMsg(player_guid, u1, u2) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)
      sendResponse(PacketCoding.CreateGamePacket(0, DismountVehicleMsg(player_guid, u1, true))) //should be safe; replace with ObjectDetachMessage later

    case msg@SquadDefinitionActionMessage(a, b, c, d, e, f, g, h, i) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg@AvatarGrenadeStateMessage(player_guid, state) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg@GenericCollisionMsg(u1, p, t, php, thp, pv, tv, ppos, tpos, u2, u3, u4) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(p)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        if (!player.spectator) {
          if (player.redHealth - 10 <= 0) player.redHealth = 1
          if (player.redHealth - 10 > 0) player.redHealth -= 10
          if (player.blueArmor - 20 <= 0) player.blueArmor = 0
          if (player.blueArmor - 20 > 0) player.blueArmor -= 20
          avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid),0,player.redHealth)
          avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid),4,player.blueArmor)
        }
      }

    case msg@BugReportMessage(version_major, version_minor, version_date, bug_type, repeatable, location, zone, pos, summary, desc) =>
      log.info("ID: " + sessionId + " " + msg)

    case msg@BindPlayerMessage(action, bindDesc, unk1, logging, unk2, unk3, unk4, pos) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg @ PlanetsideAttributeMessage(avatar_guid, attribute_type, attribute_value) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)
      //      sendResponse(PacketCoding.CreateGamePacket(0,PlanetsideAttributeMessage(avatar_guid, attribute_type, attribute_value)))
      avatarService ! AvatarService.PlanetsideAttribute(avatar_guid,attribute_type,attribute_value)

    case msg @ BattleplanMessage(char_id, player_name, zonr_id, diagrams) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg @ CreateShortcutMessage(player_guid, slot, unk, add, shortcut) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg @ FriendsRequest(action, friend) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case msg@HitHint(source_guid,player_guid) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(source_guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        val OnlinePlayer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(player_guid)
        if (OnlinePlayer.isDefined && !player.spectator) {
          val onlineplayer: PlayerAvatar = OnlinePlayer.get
          if ( !onlineplayer.spectator && onlineplayer.continent == "i2"){
            avatarService ! AvatarService.HitHintReturn(source_guid, player_guid)
            val distanceBetweenPlayers : Float = distance(player.getPosition, onlineplayer.getPosition)
            var currentDamage : Int = 0
            var currentResistance : Int = 0

            // Weapons damages
            if (player.getUsedHolster != 4) {
              if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "r_shotgun" && player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 0) { // JH normal
                currentDamage = damages(shotgun_shell_velocity, shotgun_shell_lifespan, shotgun_shell_lifespan, 0.0f, shotgun_shell_damage0 + 1, distanceBetweenPlayers)
              }
              else if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "r_shotgun" && player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 1) { // JH 3-shot
                currentDamage = damages(shotgun_shell_velocity, shotgun_shell_lifespan, shotgun_shell_lifespan, 0.0f, shotgun_shell_damage0 - 3, distanceBetweenPlayers)
              }
              else if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "gauss") { // Gauss
                currentDamage = damages(bullet_9mm_velocity, bullet_9mm_lifespan, bullet_9mm_degrade_delay, bullet_9mm_degrade_multiplier, bullet_9mm_damage0 + 2, distanceBetweenPlayers)
              }
              if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "mini_chaingun" && player.getEquipmentInHolster(player.getUsedHolster).get.ammoTypeIndex == 0) { // MCG normal
                currentDamage = damages(bullet_9mm_velocity, bullet_9mm_lifespan, bullet_9mm_degrade_delay, bullet_9mm_degrade_multiplier, bullet_9mm_damage0, distanceBetweenPlayers)
              }
              else if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "mini_chaingun" && player.getEquipmentInHolster(player.getUsedHolster).get.ammoTypeIndex == 1) { // MCG AP
                currentDamage = damages(bullet_9mm_velocity, bullet_9mm_lifespan, bullet_9mm_degrade_delay, bullet_9mm_degrade_multiplier, bullet_9mm_AP_damage0, distanceBetweenPlayers)
              }
              else if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "cycler") { // Cycler
                currentDamage = damages(bullet_9mm_velocity, bullet_9mm_lifespan, bullet_9mm_degrade_delay, bullet_9mm_degrade_multiplier, bullet_9mm_damage0, distanceBetweenPlayers)
              }
              if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "lasher" && player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 0) { // Lasher normal
                currentDamage = damages(lasher_projectile_velocity, lasher_projectile_lifespan, lasher_projectile_degrade_delay, lasher_projectile_degrade_multiplier, lasher_projectile_damage0, distanceBetweenPlayers)
              }
              else if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "lasher" && player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 1) { // Lasher AP
                currentDamage = damages(lasher_projectile_velocity, lasher_projectile_lifespan, lasher_projectile_degrade_delay, lasher_projectile_degrade_multiplier, lasher_projectile_AP_damage0, distanceBetweenPlayers)
              }
              else if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "pulsar" && player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 0) { // Pulsar normal
                currentDamage = damages(pulsar_projectile_velocity, pulsar_projectile_lifespan, pulsar_projectile_degrade_delay, pulsar_projectile_degrade_multiplier, pulsar_projectile_damage0, distanceBetweenPlayers)
              }
              else if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "pulsar" && player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 1) { // Pulsar AP
                currentDamage = damages(pulsar_projectile_velocity, pulsar_projectile_lifespan, pulsar_projectile_AP_degrade_delay, pulsar_projectile_AP_degrade_multiplier, pulsar_projectile_AP_damage0, distanceBetweenPlayers)
              }
              if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "flechette") { // Sweeper
                currentDamage = damages(shotgun_shell_velocity, shotgun_shell_lifespan, shotgun_shell_lifespan, 0.0f, shotgun_shell_damage0, distanceBetweenPlayers)
              }
            }
            else { // knife
              if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "katana") { // light saber
                if (player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 0) {
                  currentDamage = damages(melee_ammo_projectile_velocity, melee_ammo_projectile_lifespan, melee_ammo_projectile_lifespan, 0.0f, melee_ammo_projectile_damage, distanceBetweenPlayers)
                }
                else if (player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 1) {
                  currentDamage = damages(melee_ammo_projectile_velocity, melee_ammo_projectile_lifespan, melee_ammo_projectile_lifespan, 0.0f, melee_ammo_projectile_damage, distanceBetweenPlayers)
                }
              }
              else {
                if (player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 0) {
                  currentDamage = damages(melee_ammo_projectile_velocity, melee_ammo_projectile_lifespan, melee_ammo_projectile_lifespan, 0.0f, melee_ammo_projectile_damage, distanceBetweenPlayers)
                }
                else if (player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 1) {
                  currentDamage = damages(melee_ammo_projectile_velocity, melee_ammo_projectile_lifespan, melee_ammo_projectile_lifespan, 0.0f, chainblade_projectile_damage, distanceBetweenPlayers)
                }
              }
            }

            if (onlineplayer.getExoSuitType == ExoSuitType.Agile) {
              currentResistance = lite_armor_resistance_direct
            }
            else if (onlineplayer.getExoSuitType == ExoSuitType.Reinforced) {
              currentResistance = med_armor_resistance_direct
            }

            onlineplayer.redHealth = damagesAfterResist(currentDamage, currentResistance, onlineplayer.redHealth, onlineplayer.blueArmor)._1
            onlineplayer.blueArmor = damagesAfterResist(currentDamage, currentResistance, onlineplayer.redHealth, onlineplayer.blueArmor)._2
            avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(onlineplayer.guid), 0, onlineplayer.redHealth)
            avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(onlineplayer.guid), 4, onlineplayer.blueArmor)
            if (onlineplayer.redHealth == 0) {
              onlineplayer.death_by = player.guid
            }
          }
        }
      }

    case msg@SpawnRequestMessage(u1, u2, u3, u4, u5) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        sendResponse(PacketCoding.CreateGamePacket(0, AvatarDeadStateMessage(3,1000,1000,player.getPosition,0,true)))
        Transfer.disposeSelf(traveler,sessionId)
        avatarService ! AvatarService.unLoadMap(PlanetSideGUID(player.guid))
        if (player.faction == PlanetSideEmpire.NC && u2 == 6) {
          player.setPosition(Vector3(2238.0078f,2133.586f,101.296875f))
        }
        else if (player.faction == PlanetSideEmpire.NC && u2 == 7) {
          player.setPosition(Vector3(2629.461f,2278.3672f,84.359375f))
        }
        else if (player.faction == PlanetSideEmpire.TR && u2 == 6) {
          player.setPosition(Vector3(2048.7344f,2325.3672f,105.0625f))
        }
        else if (player.faction == PlanetSideEmpire.TR && u2 == 7) {
          player.setPosition(Vector3(1804f,2693f,82f))
        }
        else if (player.faction == PlanetSideEmpire.VS && u2 == 6) {
          player.setPosition(Vector3(1882.4766f,2040.6406f,101.484375f))
        }
        else if (player.faction == PlanetSideEmpire.VS && u2 == 7) {
          player.setPosition(Vector3(1728f,1740f,82f))
        }
        player.redHealth = player.getMaxHealth
        player.blueArmor = player.getMaxPersonalArmor
        Thread.sleep(1000)
        Transfer.loadSelf(traveler, sessionId, (player.getPosition.x.toInt,player.getPosition.y.toInt,player.getPosition.z.toInt))
        avatarService ! AvatarService.LoadMap(PlanetSideGUID(player.guid))
        sendResponse(PacketCoding.CreateGamePacket(0, AvatarDeadStateMessage(0,0,0,player.getPosition,0,true)))
      }
    case msg@ReleaseAvatarRequestMessage() =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        sendResponse(PacketCoding.CreateGamePacket(0, AvatarDeadStateMessage(2,0,0,player.getPosition,0,true)))
        avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(player.guid),6,1)
      }

    case msg@LashMessage(seq_time, killer, victim, bullet, pos, unk1) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(killer)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        if (ServerInfo.getLog) log.info("ID: " + sessionId + " / " + player.name + " (" + player.faction + ") " + player.continent + "-" + player.posX.toInt + "/" + player.posY.toInt + "/" + player.posZ.toInt + " " + msg)
        val OnlinePlayer: Option[PlayerAvatar] = PlayerMasterList.getPlayer(victim)
        if (OnlinePlayer.isDefined && !player.spectator) {
          val onlineplayer: PlayerAvatar = OnlinePlayer.get
          if ( !onlineplayer.spectator && onlineplayer.continent == "i2"){
            avatarService ! AvatarService.HitHintReturn(killer, victim)
            val distanceBetweenPlayers : Float = distance(player.getPosition, onlineplayer.getPosition)
            var currentDamage : Int = 0
            var currentResistance : Int = 0
            // Lasher damages
            if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "lasher" && player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 0) { // Lasher normal
              currentDamage = damages(lasher_projectile_velocity, lasher_projectile_lifespan, lasher_projectile_degrade_delay, lasher_projectile_degrade_multiplier, lasher_projectile_damage0, distanceBetweenPlayers)
            }
            else if (player.getEquipmentInHolster(player.getUsedHolster).get.getName == "lasher" && player.getEquipmentInHolster(player.getUsedHolster).get.fireModeIndex == 1) { // Lasher AP
              currentDamage = damages(lasher_projectile_velocity, lasher_projectile_lifespan, lasher_projectile_degrade_delay, lasher_projectile_degrade_multiplier, lasher_projectile_AP_damage0, distanceBetweenPlayers)
            }
            if (onlineplayer.getExoSuitType == ExoSuitType.Agile) {
              currentResistance = lite_armor_resistance_direct
            }
            else if (onlineplayer.getExoSuitType == ExoSuitType.Reinforced) {
              currentResistance = med_armor_resistance_direct
            }
            // currentDamage * 0.2 for lash damages
            onlineplayer.redHealth = damagesAfterResist((currentDamage*0.2).toInt, currentResistance, onlineplayer.redHealth, onlineplayer.blueArmor)._1
            onlineplayer.blueArmor = damagesAfterResist((currentDamage*0.2).toInt, currentResistance, onlineplayer.redHealth, onlineplayer.blueArmor)._2
            avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(onlineplayer.guid), 0, onlineplayer.redHealth)
            avatarService ! AvatarService.PlanetsideAttribute(PlanetSideGUID(onlineplayer.guid), 4, onlineplayer.blueArmor)
            if (onlineplayer.redHealth == 0) {
              onlineplayer.death_by = player.guid
            }
          }
        }
      }

    case msg @ TargetingImplantRequest(list) =>
      if (ServerInfo.getLog) log.info("ID: " + sessionId + " " + msg)

    case default => log.info("ID: " + sessionId + s" Unhandled GamePacket ${pkt} ")
  }

  def failWithError(error: String) = {
    log.error(error)
    sendResponse(PacketCoding.CreateControlPacket(ConnectionClose()))
  }

  def sendResponse(cont: PlanetSidePacketContainer): Unit = {
//    log.info("WORLD SEND_" + sessionId + " " + cont)
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

  def distance(pos1 : Vector3, pos2 : Vector3) : Float = {
    math.sqrt(math.pow(pos1.x-pos2.x,2)+math.pow(pos1.y-pos2.y,2)+math.pow(pos1.z-pos2.z,2)).toFloat
  }

  def damages(velocity : Int, lifespan : Float, degrade_delay : Float, degrade_multiplier : Float, damage0 : Int, distance : Float) : Int = {
    val distanceMax : Float = lifespan * velocity
    val distanceNoDegrade : Float = degrade_delay * velocity
    var damage : Int = 0
    if (distance <= distanceNoDegrade && distance <= distanceMax) {
      damage = damage0
    }
    else if (distance > distanceNoDegrade && distance <= distanceMax) {
      damage = (damage0 - (distance - distanceNoDegrade) * degrade_multiplier).toInt
      if (damage < 6) damage = 6
    }
    damage
  }
  def damagesAfterResist(damages : Int, resistance : Int, currentHP : Int, currentArmor : Int) : (Int, Int) = {
    var newHP : Int = currentHP
    var newArmor : Int = currentArmor
    if (damages != 0) {
      if (currentArmor >= resistance) {
        newArmor = currentArmor - resistance
        if (damages < resistance) {
          newHP = currentHP
        }
        else {
          newHP = currentHP - damages + resistance
        }
      }
      else if (currentArmor < resistance && currentArmor >= 0) {
        newArmor = 0
        newHP = currentHP - damages + currentArmor
      }
      if(newHP < 0) newHP = 0
    }
    (newHP,newArmor)
  }

  def discordROF(guid : PlanetSideGUID, name : String) = {
    log.info("Cheat player (ROF) : " + name + " ID : " + guid.guid)
    sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(guid, 15, 600)))
    sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_NOTE,true,"Live Server","Rate of fire hack detected. If you feel this may be in error, please report the weapon you are using in the Discord #bug-report channel.",Some(""))))
  }
  def discordPullH(guid : PlanetSideGUID, name : String) = {
    log.info("Cheat player (PullHack) : " + name + " ID : " + guid.guid)
//    sendResponse(PacketCoding.CreateGamePacket(0, PlanetsideAttributeMessage(guid, 15, 600)))
//    sendResponse(PacketCoding.CreateGamePacket(0, ChatMsg(ChatMessageType.CMT_NOTE,true,"Live Server","Pull hack detected. If you feel this may be in error, please report the weapon you are using in the Discord #bug-report channel.",Some(""))))
  }
}