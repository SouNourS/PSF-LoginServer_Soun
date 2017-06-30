// Copyright (c) 2016 PSForever.net to present
import akka.actor.Actor
import akka.event.{ActorEventBus, SubchannelClassification}
import akka.util.Subclassification
import net.psforever.packet.game.PlanetSideGUID
import net.psforever.types.Vector3

object G_EnvironmentService {
  case class Join()
  case class Leave()
  case class LeaveAll()
}

/*
   /G_Environment/
   EX : change base's status on another cont
 */

final case class G_EnvironmentMessage(to : String = "", function : String = "",
                                      GUID1 : PlanetSideGUID = PlanetSideGUID(0), GUID2 : PlanetSideGUID, GUID3 : PlanetSideGUID = PlanetSideGUID(0),
                                      INT1 : Int = 0, INT2 : Int = 0, INT3 : Int = 0, INT4 : Int = 0, INT5 : Int = 0,
                                      FLOAT1 : Float = 0f, FLOAT2 : Float = 0f, FLOAT3 : Float = 0f, FLOAT4 : Float = 0f, FLOAT5 : Float = 0f,
                                      POS1 : Vector3 = Vector3(0f,0f,0f), POS2 : Vector3 = Vector3(0f,0f,0f), POS3 : Vector3 = Vector3(0f,0f,0f), POS4 : Vector3 = Vector3(0f,0f,0f), POS5 : Vector3 = Vector3(0f,0f,0f),
                                      Bool1 : Boolean = false, Bool2 : Boolean = false, Bool3 : Boolean = false, Bool4 : Boolean = false, Bool5 : Boolean = false,
                                      Long1 : Long = 0, Long2 : Long = 0, Long3 : Long = 0, Long4 : Long = 0, Long5 : Long = 0)

class G_EnvironmentEventBus extends ActorEventBus with SubchannelClassification {
  type Event = G_EnvironmentMessage
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

class G_EnvironmentService extends Actor {
  import G_EnvironmentService._
  private [this] val log = org.log4s.getLogger

  override def preStart = {
    log.info("Starting...")
  }

  val G_EnvironmentEvents = new G_EnvironmentEventBus

  /*val channelMap = Map(
    G_EnvironmentMessageType.CMT_OPEN -> G_EnvironmentPath("local")
  )*/

  def receive = {
    case Join() =>
      val path = "/G_Env/"
      val who = sender()

      log.info(s"${who} has joined ${path}")

      G_EnvironmentEvents.subscribe(who, path)
    case Leave() =>
      G_EnvironmentEvents.unsubscribe(sender())
    case LeaveAll() =>
      G_EnvironmentEvents.unsubscribe(sender())

    case _ =>
  }
}
