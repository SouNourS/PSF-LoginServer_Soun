// Copyright (c) 2016 PSForever.net to present
import akka.actor.{Actor, ActorIdentity, ActorRef, ActorSystem, Identify, Props}

import scala.collection.mutable

object ServiceManager2 {
  var serviceManager2 = Actor.noSender

  def boot(implicit system : ActorSystem) = {
    serviceManager2 = system.actorOf(Props[ServiceManager2], "service2")
    serviceManager2
  }

  case class Register(props : Props, name : String)
  case class Lookup2(name : String)
  case class LookupResult(endpoint : ActorRef)
}

class ServiceManager2 extends Actor {
  import ServiceManager2._
  private [this] val log = org.log4s.getLogger

  var nextLookupId : Long = 0
  val lookups : mutable.Map[Long, ActorRef] = mutable.Map()

  override def preStart = {
    log.info("Starting...")
  }

  def receive = {
    case Register(props, name) =>
      log.info(s"Registered ${name} service")
      context.actorOf(props, name)
    case Lookup2(name) =>
      context.actorSelection(name) ! Identify(nextLookupId)
      lookups += (nextLookupId -> sender())
      nextLookupId += 1
    case ActorIdentity(id, ref) =>
      val idNumber = id.asInstanceOf[Long]

      if(lookups contains idNumber) {
        lookups(idNumber) ! LookupResult(ref.get)
        lookups.remove(idNumber)
      }
    case default =>
      log.error(s"Invalid message received ${default}")
  }
}
