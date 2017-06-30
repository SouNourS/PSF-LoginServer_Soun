// Copyright (c) 2016 PSForever.net to present
import akka.actor.Actor
import akka.event.{ActorEventBus, SubchannelClassification}
import akka.util.Subclassification
import net.psforever.objects.{PlayerAvatar, PlayerMasterList}
import net.psforever.packet.game.PlanetSideGUID
import net.psforever.types.Vector3

object WeaponService {
  case class Join(channel : String)
  case class Leave()
  case class LeaveAll()
  case class ChangeFireState(itemID : PlanetSideGUID, sessionId : Long)
  case class ChangeFireMode(item_GUID : PlanetSideGUID, fire_mode : Int, sessionId : Long)
  case class ReloadMsg(item_GUID : PlanetSideGUID, ammo_clip2 : Int, sessionId : Long)
  case class ChangeAmmoMode(item_GUID : PlanetSideGUID, sessionId : Long)
}

/*
   /weapon/
 */

final case class WeaponMessage(to : String = "", function : String = "",
                               GUID1 : PlanetSideGUID = PlanetSideGUID(0), GUID2 : PlanetSideGUID, GUID3 : PlanetSideGUID = PlanetSideGUID(0),
                               INT1 : Int = 0, INT2 : Int = 0, INT3 : Int = 0, INT4 : Int = 0, INT5 : Int = 0,
                               FLOAT1 : Float = 0f, FLOAT2 : Float = 0f, FLOAT3 : Float = 0f, FLOAT4 : Float = 0f, FLOAT5 : Float = 0f,
                               POS1 : Vector3 = Vector3(0f,0f,0f), POS2 : Vector3 = Vector3(0f,0f,0f), POS3 : Vector3 = Vector3(0f,0f,0f), POS4 : Vector3 = Vector3(0f,0f,0f), POS5 : Vector3 = Vector3(0f,0f,0f),
                               Bool1 : Boolean = false, Bool2 : Boolean = false, Bool3 : Boolean = false, Bool4 : Boolean = false, Bool5 : Boolean = false,
                               Long1 : Long = 0, Long2 : Long = 0, Long3 : Long = 0, Long4 : Long = 0, Long5 : Long = 0)

class WeaponEventBus extends ActorEventBus with SubchannelClassification {
  type Event = WeaponMessage
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

class WeaponService extends Actor {
  import WeaponService._
  private [this] val log = org.log4s.getLogger

  override def preStart = {
    log.info("Starting...")
  }

  val WeaponEvents = new WeaponEventBus

  /*val channelMap = Map(
    WeaponMessageType.CMT_OPEN -> WeaponPath("local")
  )*/

  def receive = {
    case Join(channel) =>
      val path = "/Weapon/" + channel
      val who = sender()

      log.info(s"${who} has joined ${path}")

      WeaponEvents.subscribe(who, path)
    case Leave() =>
      WeaponEvents.unsubscribe(sender())
    case LeaveAll() =>
      WeaponEvents.unsubscribe(sender())
    case m @ ChangeFireState(itemID, sessionId) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        WeaponEvents.publish(WeaponMessage("/Weapon/" + player.continent, "ChangeFireState", PlanetSideGUID(itemID.guid), PlanetSideGUID(player.guid)))
      }
    case m @ ChangeFireMode(item_guid, fire_mode, sessionId) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        WeaponEvents.publish(WeaponMessage("/Weapon/" + player.continent, "ChangeFireMode", item_guid, PlanetSideGUID(player.guid),PlanetSideGUID(0),fire_mode))
      }
    case m @ ReloadMsg(item_guid, ammo_clip2, sessionId) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        WeaponEvents.publish(WeaponMessage("/Weapon/" + player.continent, "ReloadMsg", item_guid, PlanetSideGUID(player.guid),PlanetSideGUID(0),ammo_clip2))
      }
    case m @ ChangeAmmoMode(item_guid, sessionId) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(sessionId)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        WeaponEvents.publish(WeaponMessage("/Weapon/" + player.continent, "ChangeAmmoMode", item_guid, PlanetSideGUID(player.guid)))
      }
    case _ =>
  }
}
