// Copyright (c) 2016 PSForever.net to present
import akka.actor.Actor
import akka.event.{ActorEventBus, SubchannelClassification}
import akka.util.Subclassification
import net.psforever.objects.{PlayerAvatar, PlayerMasterList}
import net.psforever.packet.game.{ObjectCreateMessage, PlanetSideGUID, PlayerStateMessageUpstream}
import net.psforever.types.Vector3

object AvatarService {
  case class Join(channel : String)
  case class Leave()
  case class LeaveAll()
  case class PlayerStateMessage(msg : PlayerStateMessageUpstream)
  case class LoadMap(msg : PlanetSideGUID)
  case class unLoadMap(msg : PlanetSideGUID)
  case class ObjectHeld(msg : PlanetSideGUID)
  case class ChangeFireState(itemID : PlanetSideGUID, sessionId : Long)
  case class PlanetsideAttribute(guid : PlanetSideGUID, attribute_type : Int, attribute_value : Long)
  case class PlayerStateShift(killer : PlanetSideGUID, victim : PlanetSideGUID)
  case class DestroyDisplay(killer : PlanetSideGUID, victim : PlanetSideGUID)
  case class HitHintReturn(killer : PlanetSideGUID, victim : PlanetSideGUID)
  case class ChangeFireMode(item_GUID : PlanetSideGUID, fire_mode : Int, sessionId : Long)
  case class ReloadMsg(item_GUID : PlanetSideGUID, ammo_clip2 : Int, sessionId : Long)
  case class ChangeAmmoMode(item_GUID : PlanetSideGUID, sessionId : Long)
  case class ChangeWeapon(term_GUID : PlanetSideGUID, unk1 : Int, sessionId : Long)
  case class Doors(guid : PlanetSideGUID, doors : Int)
}

/*
   /avatar/
 */

final case class AvatarMessage(to : String = "", function : String = "", itemID : PlanetSideGUID = PlanetSideGUID(0),
                               avatar_guid : PlanetSideGUID, pos : Vector3 = Vector3(0f,0f,0f), vel : Option[Vector3] = None,
                               facingYaw : Int = 0, facingPitch : Int = 0, facingYawUpper : Int = 0,
                               is_crouching : Boolean = false, is_jumping : Boolean = false, jump_thrust : Boolean = false, is_cloaked : Boolean = false,
                               Long : Long = 0)

class AvatarEventBus extends ActorEventBus with SubchannelClassification {
  type Event = AvatarMessage
  type Classifier = String

  protected def classify(event: Event): Classifier = event.to

  protected def subclassification = new Subclassification[Classifier] {
    def isEqual(x: Classifier, y: Classifier) = x == y
    def isSubclass(x: Classifier, y: Classifier) = x.startsWith(y)
  }

  protected def publish(event: Event, subscriber: Subscriber): Unit = {
    subscriber ! event
  }
}

class AvatarService extends Actor {
  import AvatarService._
  private [this] val log = org.log4s.getLogger

  override def preStart = {
    log.info("Starting...")
  }

  val AvatarEvents = new AvatarEventBus

  /*val channelMap = Map(
    AvatarMessageType.CMT_OPEN -> AvatarPath("local")
  )*/

  def receive = {
    case Join(channel) =>
      val path = "/Avatar/" + channel
      val who = sender()

      log.info(s"${who} has joined ${path}")

      AvatarEvents.subscribe(who, path)
    case Leave() =>
      AvatarEvents.unsubscribe(sender())
    case LeaveAll() =>
      AvatarEvents.unsubscribe(sender())
    case m @ PlayerStateMessage(msg) =>
//      log.info(s"NEW: ${m}")
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(msg.avatar_guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "PlayerStateMessage", PlanetSideGUID(0),
          msg.avatar_guid, msg.pos, msg.vel, msg.facingYaw, msg.facingPitch, msg.facingYawUpper, msg.is_crouching, msg.is_jumping, msg.jump_thrust, msg.is_cloaked))
      }
    case m @ LoadMap(msg) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(msg.guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "LoadMap",PlanetSideGUID(0), PlanetSideGUID(msg.guid)))
      }
    case m @ unLoadMap(msg) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(msg.guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "unLoadMap",PlanetSideGUID(0), PlanetSideGUID(msg.guid)))
      }
    case m @ ObjectHeld(msg) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(msg.guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "ObjectHeld",PlanetSideGUID(0), PlanetSideGUID(msg.guid)))
      }
    case m @ ChangeFireState(itemID, sessionId) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "ChangeFireState", PlanetSideGUID(itemID.guid), PlanetSideGUID(player.guid)))
      }
    case m @ PlanetsideAttribute(guid, attribute_type, attribute_value) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "PlanetsideAttribute", PlanetSideGUID(0),
          guid, Vector3(0f,0f,0f), None, 0, 0, attribute_type, false, false, false, false, attribute_value))
      }
    case m @ PlayerStateShift(killer,guid) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "PlayerStateShift", killer, guid))
      }
    case m @ DestroyDisplay(killer, victim) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(victim)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "DestroyDisplay", killer, victim))
      }
    case m @ HitHintReturn(source_guid,victim_guid) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(source_guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "HitHintReturn", source_guid, victim_guid))
      }
    case m @ ChangeFireMode(item_guid, fire_mode, sessionId) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "ChangeFireMode", item_guid, PlanetSideGUID(player.guid),Vector3(0f,0f,0f),None,fire_mode))
      }
    case m @ ReloadMsg(item_guid, ammo_clip2, sessionId) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "ReloadMsg", item_guid, PlanetSideGUID(player.guid),Vector3(0f,0f,0f),None,ammo_clip2))
      }
    case m @ ChangeAmmoMode(item_guid, sessionId) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "ChangeAmmoMode", item_guid, PlanetSideGUID(player.guid)))
      }
    case m @ ChangeWeapon(term_guid, unk1, sessionId) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "ChangeWeapon", term_guid, PlanetSideGUID(player.guid),Vector3(0f,0f,0f),None,unk1))
      }
    case m @ Doors(guid, doorsID) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "Doors", PlanetSideGUID(0),
          guid, Vector3(0f,0f,0f), None, 0, 0, doorsID))
      }
    case _ =>
  }
}
