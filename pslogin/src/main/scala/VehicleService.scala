// Copyright (c) 2016 PSForever.net to present
import akka.actor.Actor
import akka.event.{ActorEventBus, SubchannelClassification}
import akka.util.Subclassification
import net.psforever.objects.{PlayerAvatar, PlayerMasterList}
import net.psforever.packet.game.{PlanetSideGUID, PlayerStateMessageUpstream, VehicleStateMessage}
import net.psforever.types.Vector3

object VehicleService {
  case class Join(channel : String)
  case class Leave()
  case class LeaveAll()
  case class LoadMap(msg : PlanetSideGUID)
  case class unLoadMap(msg : PlanetSideGUID)
  case class PlanetsideAttribute(guid : PlanetSideGUID, attribute_type : Int, attribute_value : Long)
  case class VehicleState(msg : VehicleStateMessage)
}

/*
   /vehicle/
 */

final case class VehicleMessage(to : String = "", function : String = "",
                                GUID1 : PlanetSideGUID = PlanetSideGUID(0), GUID2 : PlanetSideGUID, GUID3 : PlanetSideGUID = PlanetSideGUID(0),
                                INT1 : Int = 0, INT2 : Int = 0, INT3 : Int = 0, INT4 : Int = 0, INT5 : Int = 0,
                                FLOAT1 : Float = 0f, FLOAT2 : Float = 0f, FLOAT3 : Float = 0f, FLOAT4 : Float = 0f, FLOAT5 : Float = 0f,
                                POS1 : Vector3 = Vector3(0f,0f,0f), POS2 : Vector3 = Vector3(0f,0f,0f), POS3 : Vector3 = Vector3(0f,0f,0f), POS4 : Vector3 = Vector3(0f,0f,0f), POS5 : Vector3 = Vector3(0f,0f,0f),
                                Bool1 : Boolean = false, Bool2 : Boolean = false, Bool3 : Boolean = false, Bool4 : Boolean = false, Bool5 : Boolean = false,
                                Long1 : Long = 0, Long2 : Long = 0, Long3 : Long = 0, Long4 : Long = 0, Long5 : Long = 0,
                                OptionVect1 : Option[Vector3] = None, OptionVect2 : Option[Vector3] = None, OptionInt1 : Option[Int] = None, OptionInt2 : Option[Int] = None,
                                OptionFloat1 : Option[Float] = None, OptionFloat2 : Option[Float] = None)

class VehicleEventBus extends ActorEventBus with SubchannelClassification {
  type Event = VehicleMessage
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

class VehicleService extends Actor {
  import VehicleService._
  private [this] val log = org.log4s.getLogger

  override def preStart = {
    log.info("Starting...")
  }

  val VehicleEvents = new VehicleEventBus

  /*val channelMap = Map(
    VehicleMessageType.CMT_OPEN -> VehiclePath("local")
  )*/

  def receive = {
    case Join(channel) =>
      val path = "/Vehicle/" + channel
      val who = sender()

      log.info(s"${who} has joined ${path}")

      VehicleEvents.subscribe(who, path)
    case Leave() =>
      VehicleEvents.unsubscribe(sender())
    case LeaveAll() =>
      VehicleEvents.unsubscribe(sender())

    case m @ LoadMap(msg) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(msg.guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        VehicleEvents.publish(VehicleMessage("/Vehicle/" + player.continent, "LoadMap",PlanetSideGUID(0), PlanetSideGUID(msg.guid)))
      }
    case m @ unLoadMap(msg) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(msg.guid)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        VehicleEvents.publish(VehicleMessage("/Vehicle/" + player.continent, "unLoadMap",PlanetSideGUID(0), PlanetSideGUID(msg.guid)))
      }
    case m @ PlanetsideAttribute(guid, attribute_type, attribute_value) =>
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(guid.guid - 90)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        VehicleEvents.publish(VehicleMessage("/Vehicle/" + player.continent, "PlanetsideAttribute", PlanetSideGUID(0),
          guid,PlanetSideGUID(0),
          attribute_type,0,0,0,0,
              0f,0f,0f,0f,0f,
              Vector3(0f,0f,0f),Vector3(0f,0f,0f),Vector3(0f,0f,0f),Vector3(0f,0f,0f),Vector3(0f,0f,0f),
              false,false,false,false,false,
          attribute_value))
      }
    case m @ VehicleState(msg) =>
      //      log.info(s"NEW: ${m}")
      val playerOpt: Option[PlayerAvatar] = PlayerMasterList.getPlayer(msg.vehicle_guid.guid - 90)
      if (playerOpt.isDefined) {
        val player: PlayerAvatar = playerOpt.get
        VehicleEvents.publish(VehicleMessage("/Vehicle/" + player.continent, "VehicleState", PlanetSideGUID(0),
          msg.vehicle_guid, PlanetSideGUID(0), msg.unk1, msg.unk3, msg.unk4, msg.wheel_direction, 0, msg.ang.x,msg.ang.y,msg.ang.z,0f,0f,
          msg.pos,Vector3(0f,0f,0f),Vector3(0f,0f,0f),Vector3(0f,0f,0f),Vector3(0f,0f,0f),
          msg.unk5,msg.unk6,false,false,false,0,0,0,0,0,msg.vel,None,msg.unk2))
      }

//      EX :
//        VehicleMessage("","",
//      PlanetSideGUID(0),PlanetSideGUID(0),PlanetSideGUID(0),
//      0,0,0,0,0,
//      0f,0f,0f,0f,0f,
//      Vector3(0f,0f,0f),Vector3(0f,0f,0f),Vector3(0f,0f,0f),Vector3(0f,0f,0f),Vector3(0f,0f,0f),
//      false,false,false,false,false,
//      0L,0L,0L,0L,0L)
    case _ =>
  }
}
