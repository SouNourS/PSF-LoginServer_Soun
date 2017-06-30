// Copyright (c) 2016 PSForever.net to present
import akka.actor.Actor
import akka.event.{ActorEventBus, SubchannelClassification}
import akka.util.Subclassification
import net.psforever.objects.{PlayerAvatar, PlayerMasterList}
import net.psforever.packet.game.PlanetSideGUID
import net.psforever.types.Vector3

object L_EnvironmentService {
  case class Join(channel : String)
  case class Leave()
  case class LeaveAll()
  case class Doors(guid : PlanetSideGUID, doors : PlanetSideGUID)
}

/*
   /L_Environment/
 */

final case class L_EnvironmentMessage(to : String = "", function : String = "",
                                      GUID1 : PlanetSideGUID = PlanetSideGUID(0), GUID2 : PlanetSideGUID, GUID3 : PlanetSideGUID = PlanetSideGUID(0),
                                      INT1 : Int = 0, INT2 : Int = 0, INT3 : Int = 0, INT4 : Int = 0, INT5 : Int = 0,
                                      FLOAT1 : Float = 0f, FLOAT2 : Float = 0f, FLOAT3 : Float = 0f, FLOAT4 : Float = 0f, FLOAT5 : Float = 0f,
                                      POS1 : Vector3 = Vector3(0f,0f,0f), POS2 : Vector3 = Vector3(0f,0f,0f), POS3 : Vector3 = Vector3(0f,0f,0f), POS4 : Vector3 = Vector3(0f,0f,0f), POS5 : Vector3 = Vector3(0f,0f,0f),
                                      Bool1 : Boolean = false, Bool2 : Boolean = false, Bool3 : Boolean = false, Bool4 : Boolean = false, Bool5 : Boolean = false,
                                      Long1 : Long = 0, Long2 : Long = 0, Long3 : Long = 0, Long4 : Long = 0, Long5 : Long = 0)

class L_EnvironmentEventBus extends ActorEventBus with SubchannelClassification {
  type Event = L_EnvironmentMessage
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

class L_EnvironmentService extends Actor {
  import L_EnvironmentService._
  private [this] val log = org.log4s.getLogger

  override def preStart = {
    log.info("Starting...")
  }

  val L_EnvironmentEvents = new L_EnvironmentEventBus

  /*val channelMap = Map(
    L_EnvironmentMessageType.CMT_OPEN -> L_EnvironmentPath("local")
  )*/

  def receive = {
    case Join(channel) =>
      val path = "/L_Env/" + channel
      val who = sender()

      log.info(s"${who} has joined ${path}")

      L_EnvironmentEvents.subscribe(who, path)
    case Leave() =>
      L_EnvironmentEvents.unsubscribe(sender())
    case LeaveAll() =>
      L_EnvironmentEvents.unsubscribe(sender())

    case m @ Doors(player_guid, doorsID) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(player_guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        L_EnvironmentEvents.publish(L_EnvironmentMessage("/L_Env/" + player.continent, "Doors", doorsID, player_guid))
      }

    case _ =>
  }
}
