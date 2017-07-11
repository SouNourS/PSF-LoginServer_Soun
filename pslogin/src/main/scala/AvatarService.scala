// Copyright (c) 2016 PSForever.net to present
import akka.actor.Actor
import akka.event.{ActorEventBus, SubchannelClassification}
import akka.util.Subclassification
import net.psforever.objects.{PlayerAvatar, PlayerMasterList}
import net.psforever.packet.game.{PlanetSideGUID, PlayerStateMessageUpstream}
import net.psforever.types.Vector3

abstract case class InboundMessage()

object AvatarService {
  case class Join(channel : String)
  case class Leave()
  case class LeaveAll()
  case class PlayerStateMessage(msg : PlayerStateMessageUpstream)
  case class LoadMap(msg : PlanetSideGUID)
  case class unLoadMap(msg : PlanetSideGUID)
  case class ObjectHeld(msg : PlanetSideGUID)
  case class PlanetsideAttribute(guid : PlanetSideGUID, attribute_type : Int, attribute_value : Long)
  case class PlayerStateShift(killer : PlanetSideGUID, victim : PlanetSideGUID)
  case class DestroyDisplay(killer : PlanetSideGUID, victim : PlanetSideGUID)
  case class HitHintReturn(killer : PlanetSideGUID, victim : PlanetSideGUID)
  case class ChangeWeapon(unk1 : Int, sessionId : Long)
}

sealed trait Reply

object AvatarServiceReply {
  final case class unLoadMap() extends Reply
  final case class LoadMap() extends Reply
  final case class PlayerStateMessage(pos : Vector3, vel : Option[Vector3], facingYaw : Int, facingPitch : Int, facingUpper : Int, is_crouching : Boolean, jumping : Boolean, jthrust : Boolean, is_cloaked : Boolean) extends Reply
  final case class ObjectHeld() extends Reply
  final case class PlanetSideAttribute(facingUpper : Int, long : Long) extends Reply
  final case class PlayerStateShift(itemID : PlanetSideGUID) extends Reply
  final case class DestroyDisplay(itemID : PlanetSideGUID) extends Reply
  final case class HitHintReturn(itemID : PlanetSideGUID) extends Reply
  final case class ChangeWeapon(facingYaw : Int) extends Reply
}

/*
   /avatar/
 */

final case class AvatarMessage(to : String, avatar_guid : PlanetSideGUID, replyMessage : Reply)

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

      log.info(s"$who has joined $path")

      AvatarEvents.subscribe(who, path)
    case Leave() =>
      AvatarEvents.unsubscribe(sender())
    case LeaveAll() =>
      AvatarEvents.unsubscribe(sender())

    case AvatarService.PlayerStateMessage(msg) =>
//      log.info(s"NEW: ${m}")
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(msg.avatar_guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, msg.avatar_guid,
          AvatarServiceReply.PlayerStateMessage(msg.pos, msg.vel, msg.facingYaw, msg.facingPitch, msg.facingYawUpper, msg.is_crouching, msg.is_jumping, msg.jump_thrust, msg.is_cloaked)
        ))

      }
    case AvatarService.LoadMap(msg) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(msg.guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, PlanetSideGUID(msg.guid),
          AvatarServiceReply.LoadMap()
        ))
      }
    case AvatarService.unLoadMap(msg) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(msg.guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, PlanetSideGUID(msg.guid),
          AvatarServiceReply.unLoadMap()
        ))
      }
    case AvatarService.ObjectHeld(msg) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(msg.guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, PlanetSideGUID(msg.guid),
          AvatarServiceReply.ObjectHeld()
        ))
      }
    case AvatarService.PlanetsideAttribute(guid, attribute_type, attribute_value) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, guid,
          AvatarServiceReply.PlanetSideAttribute(attribute_type, attribute_value)
        ))
      }
    case AvatarService.PlayerStateShift(killer, guid) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, guid,
          AvatarServiceReply.PlayerStateShift(killer)
        ))
      }
    case AvatarService.DestroyDisplay(killer, victim) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(victim)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, victim,
          AvatarServiceReply.DestroyDisplay(killer)
        ))
      }
    case AvatarService.HitHintReturn(source_guid,victim_guid) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(source_guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, victim_guid,
          AvatarServiceReply.DestroyDisplay(source_guid)
        ))
      }
    case AvatarService.ChangeWeapon(unk1, sessionId) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        AvatarEvents.publish(AvatarMessage("/Avatar/" + player.continent, PlanetSideGUID(player.guid),
          AvatarServiceReply.ChangeWeapon(unk1)
        ))
      }
    case msg =>
      log.info(s"Unhandled message $msg from $sender")
  }
}
