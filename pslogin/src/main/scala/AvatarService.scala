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
}

/*
   /avatar/
 */

final case class AvatarMessage(to : String = "", function : String = "", itemID : PlanetSideGUID = PlanetSideGUID(0), avatar_guid : PlanetSideGUID, pos : Vector3 = Vector3(0f,0f,0f), vel : Option[Vector3] = None,
                               unk1 : Int = 0, aim_pitch : Int = 0, unk2 : Int = 0,
                               is_crouching : Boolean = false, unk4 : Boolean = false, is_cloaking : Boolean = false, Long : Long = 0)

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
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "PlayerStateMessage", PlanetSideGUID(0), msg.avatar_guid, msg.pos, msg.vel, msg.unk1, msg.aim_pitch, msg.unk2, msg.is_crouching, msg.is_jumping, msg.is_cloaking))
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
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, "PlanetsideAttribute", PlanetSideGUID(0), guid, Vector3(0f,0f,0f), None, 0, 0, attribute_type, false, false, false, attribute_value))
      }
    case _ =>
  }
}
