// Copyright (c) 2016 PSForever.net to present
package net.psforever.objects

import net.psforever.packet.game.PlanetSideGUID
import net.psforever.types.PlanetSideEmpire

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * A class for storing very important mappings between Vehicles, Vehicle ids, and external identifiers for Vehicles.
  */
final class VehicleMasterList {
  /**
    * When this instance was instantiated.
    */
  val started : Long = System.currentTimeMillis
  /**
    * This mapping coordinates the name of the Vehicle to the Vehicle instance itself.
    * The Vehicle is built external to this global object and must be passed into it with the appropriate function call.
    * All Vehicle instances are maintained by this mapping.
    */
  val nameToCharacter : mutable.HashMap[Int, Vehicle] = mutable.HashMap[Int, Vehicle]()
  /**
    * This mapping keeps track of an internal GUID and entrusts a certain name that GUID.
    * Whenever a name is required in context to something that happens, this external GUID would be contestually known.
    * This name follows from know that GUID via the mapping.</br>
    * The two-layered mechanism provides a means of control validation for the named Vehicle.
    * If a request for this name is made from a session providing the wrong external id, it will be rejected.
    * Considerations have been made if this name is called in context to some other event (no validation).
    */
  val externToName : mutable.HashMap[Long, Int] = mutable.HashMap[Long, Int]()

  /**
    * Get the world population, broken down by Vehicle faction.
    *
    * Exclude mods from these counts, if they are included in the same lists.
    * Include Black Ops as integrated into their normal factions.
    * @return a Tuple of three population values in order: NC, TR, VS
    */
  def getWorldPopulation : (Int, Int, Int) = {
    ( nameToCharacter.count(x => { x._2.faction == PlanetSideEmpire.NC}),
      nameToCharacter.count(x => { x._2.faction == PlanetSideEmpire.TR}),
      nameToCharacter.count(x => { x._2.faction == PlanetSideEmpire.VS}) )
  }

  def getEmpireNeed : (PlanetSideEmpire.Type) = {
    var EmpireNeed : PlanetSideEmpire.Type = PlanetSideEmpire.NC
    if (getWorldPopulation._2 <= getWorldPopulation._1) {
      EmpireNeed = PlanetSideEmpire.TR
      if (getWorldPopulation._3 < getWorldPopulation._2) {
        EmpireNeed = PlanetSideEmpire.VS
      }
    }
    EmpireNeed
  }

  /**
    * Get a Vehicle representation.
    * @param guid the transformed GUID of the Vehicle
    * @return an Option containing the Vehicle object; None, if the GUID is unknown
    */
  def getVehicle(guid : PlanetSideGUID) : Option[Vehicle] = {
    guid match {
      case PlanetSideGUID(id : Int) => getVehicle(id)
      case _ => None
    }
  }

  /**
    * Get a Vehicle representation.
    * @param id the raw GUID of the Vehicle
    * @return an Option containing the Vehicle object; None, if the id is unknown
    */
  def getVehicle(id : Int) : Option[Vehicle] = {
    nameToCharacter.get(id)
  }

  /**
    * Get a Vehicle representation.
    * @param externId an external id that should be associated with a Vehicle id
    * @return an Option containing the Vehicle object; None, if the external id is unknown
    */
  def getVehicle(externId : Long) : Option[Vehicle] = {
    val nameForSession = externToName.get(externId)
    if(nameForSession.isDefined) {
      return nameToCharacter.get(nameForSession.get)
    }
    None
  }

  /**
    * Get a Vehicle representation.
    * This is a "safe" get-method where the user who is controlling the character is required to be an origin point for the request.
    * This is the raw Int and Long overload.
    * @param id the raw GUID of the Vehicle
    * @param externId an external id that should be associated with a Vehicle id
    * @return an Option containing the user's vehicle character; None, the data is not related
    */
  def getVehicle(id : Int, externId : Long) : Option[Vehicle] = {
    val nameForSession = externToName.get(externId)
    if(nameForSession.isDefined && nameForSession.get.equals(id))
      return nameToCharacter.get(id)
    None
    //TODO flag this session for funny business -- not asking for own character
  }

  /**
    * Mark that a Vehicle with this id will be associated with this external id.
    * @param externId an external id that should be associated with a Vehicle id
    * @param id the raw GUID of the Vehicle
    * @return the raw GUID of the Vehicle being referenced by this external id; Int.MinValue, if the association fails
    */
  def userClaimsCharacter(externId : Long, id : Int) : Int = {
    externToName.filter(x => x._2 == id).foreach(x =>
      if(x._1 != externId)
        return Int.MinValue // name already reserved
    )
    externToName.filter(x => x._1 == externId).foreach(x =>
      if(x._2 != id)
        return Int.MinValue // sessionId already used
    )
    if(!nameToCharacter.contains(id))
      return Int.MinValue // sessionId already used

    externToName += (externId -> id)
    id
  }

  /**
    * Forget that a Vehicle with this id will be associated with this external id.
    * @param externId an external id that should be associated with a Vehicle id
    * @return the raw GUID of the Vehicle previously referenced by this external id; Int.MinValue, if the association fails
    */
  def userDissociatesCharacter(externId : Long) : Int = {
    val token = externToName.remove(externId)
    if(token.isDefined)
      return token.get
    Int.MinValue
  }

  /**
    * Forget that a Vehicle with this id will be associated with this external id.
    * @param externId an external id that should be associated with a Vehicle id
    * @param id the raw GUID of the Vehicle
    * @return the raw GUID of the Vehicle previously referenced by this external id; Int.MinValue, if the association fails
    */
  def userDissociatesCharacter(externId : Long, id : Int) : Int = {
    val token = externToName.get(externId)
    if(token.isDefined && token.get == id) {
      externToName.remove(externId)
      return token.get
    }
    Int.MinValue
  }

  /**
    * These Vehicles are known to the server but are not (currently) associated to any external ids.
    * @return a List of the unassociated ids
    */
  def getUnclaimedCharacters : List[Int] = {
    var unclaimed : ListBuffer[Int] = new ListBuffer[Int]
    if(externToName.size != nameToCharacter.size) {
      var claims: mutable.Map[Int, Long] = mutable.Map[Int, Long]()
      externToName.foreach { case (key: Long, value: Int) => claims += { value -> key } } // Reverse the sessionToName mapping for a quick lookup
      nameToCharacter.foreach { case (name, char) => if (!claims.contains(name)) unclaimed += name } // Collect unclaimed characters
    }
    unclaimed.toList
  }

  /**
    * Add a Vehicle.
    * @param vehicle the Vehicle
    * @return true, if this Vehicle was able to join (or re-join) the list; false, otherwise
    */
  def addVehicle(vehicle : Vehicle) : Boolean = {
    val guid : Int = vehicle.guid
    if(nameToCharacter.contains(guid)) {
      // Character already on server; is it unowned?
      if(externToName.count(x => { x._2 == guid }) > 0) {
        //TODO flag for funny business -- another external id associated with this Vehicle id
      }
      return false
    }

    nameToCharacter += (guid -> vehicle)
    true
  }

  /**
    * Add a Vehicle.
    * This is a "safe" attempt to add the vehicle.
    * It should be called when an external id is prepared to be associated with the vehicle.
    * @param vehicle the Vehicle
    * @param externId an external id that should be associated with a Vehicle id
    * @return true, if this Vehicle was able to join (or re-join) the list; false, otherwise
    */
  def addVehicle(vehicle : Vehicle, externId : Long) : Boolean = {
    val guid : Int = vehicle.guid
    if(externToName.contains(externId)) {
      if(externToName(externId) != guid) {
        //TODO flag for funny business -- prior claim to another Vehicle
        return false
      }
    }
    if(nameToCharacter.contains(guid)) {
      // vehicle already known; is it unassociated?
      if(externToName.count(x => { x._2 == guid && x._1 != externId }) > 0) {
        //TODO flag for funny business -- another external id associated with this Vehicle id
        return false
      }
      return true
    }

    nameToCharacter += (guid -> vehicle)
    externToName += (externId -> guid)
    true
  }

  /**
    * Remove a Vehicle.
    * @param vehicle the Vehicle
    * @return always returns true
    */
  def removeVehicle(vehicle : Vehicle) : Boolean = {
    removeVehicle(vehicle.guid)
  }

  /**
    * Remove a Vehicle.
    * @param guid the transformed GUID of the Vehicle
    * @return always returns true
    */
  def removeVehicle(guid : PlanetSideGUID) : Boolean = {
    guid match {
      case PlanetSideGUID(id : Int) => removeVehicle(id)
      case _ => false
    }
  }

  /**
    * Remove a Vehicle.
    * Clean up any potential mappings connected to this vehicle and its id, including its external id.
    * @param id the raw GUID of the Vehicle
    * @return true, if the Vehicle is removed; false, othwerwise
    */
  def removeVehicle(id : Int) : Boolean = {
    if(!nameToCharacter.contains(id))
      return false

    nameToCharacter.remove(id)
    val session = externToName.filter(x => {
      x._2 == id
    })
    if(session.nonEmpty) {
      // Housecleaning
      session.foreach { case (key : Long, value : Int) => userDissociatesCharacter(key) }
    }
    true
  }

  /**
    * Remove a Vehicle.
    * Clean up any potential mappings connected to this vehicle and its id, including its external id.
    * @param externId an external id that should be associated with a Vehicle id
    * @return true, if the Vehicle associated with this external id is removable; false, otherwise
    */
  def removeVehicle(externId : Long) : Boolean = {
    val token : Option[Int] = externToName.remove(externId)
    if(token.isDefined) {
      nameToCharacter.remove(token.get)
      return true
    }
    false
  }

  /**
    * Hastily remove all Vehicles from the collection and clear all mappings and external id mappings.
    * @return an unsorted list of the Vehicle ids that were still present
    */
  def shutdown : List[Vehicle] = {
    val list = nameToCharacter.values.toList
    externToName.clear
    nameToCharacter.clear
    list
  }
}

/**
  * The vehicle master list is a record that keeps track of all vehicles and characters currently logged into the server, regardless of zone.
  * For the purpose of the explanation: a "vehicle" is the end user human; a "character" is the digital representation on the server.<br>
  * The "vehicle" is known by a session id that is important to the vehicle's current connection to the server through their client.
  * The "character" is known by a character id (of some sort) that is persistent to the character.<br>
  * The master list is a really singleton.
  * <br>
  * Workflow:<br>
  * 1. Vehicle selects a character from their client's login screen (CharacterRequestMessage, Selection).<br>
  * 2. An entry mapping the character id to the character.<br>
  * 3. During the character and world setup on the client, the vehicle's session is mapped to the character id.<br>
  * 4. The vehicle is logged into the server and has control over the character once the client finishes loading.<br>
  * 5. If the vehicle times out of the server, the mapping between the session id and the character id is removed.<br>
  * 6. If the vehicle rejoins the server soon enough, the mapping between a new session id and the character id is made.<br>
  * 7. If the vehicle does not rejoin the server, eventually the mapping between the character and its id are removed.<br>
  * 8. If the vehicle timesout his character gently by voluntarily leaving, the character mapping is removed switfly.<br>
  * 9. Neither the vehicle nor the character are connected to the server anymore.<br>
  */
object VehicleMasterList {
  /**
    * An instance of the character and session records.
    * Under singleton design, there is ever only one.
    */
  private var instance : Option[VehicleMasterList] = None

  /**
    * Create a new instance of the underlying class to be returned each time, or return that already-created instance
    * @return the data structures
    */
  private def getInstance : VehicleMasterList = {
    if(instance.isEmpty)
      instance = Option(new VehicleMasterList)
    instance.get
  }

  /**
    * @see (class VehicleMasterList { ... }).getWorldPopulation
    */
  def getWorldPopulation : (Int, Int, Int) = {
    getInstance.getWorldPopulation
  }

  def getEmpireNeed : (PlanetSideEmpire.Type) = {
    getInstance.getEmpireNeed
  }

  /**
    * @see (class VehicleMasterList { ... }).getVehicle(PlanetSideGUID)
    */
  def getVehicle(name : PlanetSideGUID) : Option[Vehicle] = {
    getInstance.getVehicle(name)
  }

  /**
    * @see (class VehicleMasterList { ... }).getVehicle(Int)
    */
  def getVehicle(name : Int) : Option[Vehicle] = {
    getInstance.nameToCharacter.get(name)
  }

  /**
    * @see (class VehicleMasterList { ... }).getVehicle(Long)
    */
  def getVehicle(externId : Long) : Option[Vehicle] = {
    getInstance.getVehicle(externId)
  }

  /**
    * @see (class VehicleMasterList { ... }).getVehicle(Int, Long)
    */
  def getVehicle(name : Int, externId : Long) : Option[Vehicle] = {
    getInstance.getVehicle(name, externId)
  }

  /**
    * @see (class VehicleMasterList { ... }).userClaimsCharacter(Long, Int)
    */
  def userClaimsCharacter(externId : Long, name : Int) : Int = {
    getInstance.userClaimsCharacter(externId, name)
  }

  /**
    * @see (class VehicleMasterList { ... }).userDissociatesCharacter(Long)
    */
  def userDissociatesCharacter(externId : Long) : Int = {
    getInstance.userDissociatesCharacter(externId)
  }

  /**
    * @see (class VehicleMasterList { ... }).userDissociatesCharacter(Long, Int)
    */
  def userDissociatesCharacter(externId : Long, name : Int) : Int = {
    getInstance.userDissociatesCharacter(externId, name)
  }

  /**
    * @see (class VehicleMasterList { ... }).getUnclaimedCharacters
    */
  def getUnclaimedCharacters : List[Int] = {
    getInstance.getUnclaimedCharacters
  }

  /**
    * @see (class VehicleMasterList { ... }).addVehicle(Vehicle)
    */
  def addVehicle(vehicle : Vehicle) : Boolean = {
    getInstance.addVehicle(vehicle)
  }

  /**
    * @see (class VehicleMasterList { ... }).addVehicle(Vehicle, Long)
    */
  def addVehicle(vehicle : Vehicle, sessionId : Long) : Boolean = {
    getInstance.addVehicle(vehicle, sessionId)
  }

  /**
    * @see (class VehicleMasterList { ... }).removeVehicle(Vehicle)
    */
  def removeVehicle(vehicle : Vehicle) : Boolean = {
    getInstance.removeVehicle(vehicle.guid)
  }

  /**
    * @see (class VehicleMasterList { ... }).removeVehicle(PlanetSideGUID)
    */
  def removeVehicle(guid : PlanetSideGUID) : Boolean = {
    getInstance.removeVehicle(guid)
  }

  /**
    * @see (class VehicleMasterList { ... }).removeVehicle(Int)
    */
  def removeVehicle(guid : Int) : Boolean = {
    getInstance.removeVehicle(guid)
  }

  /**
    * @see (class VehicleMasterList { ... }).removeVehicle(Long)
    */
  def removeVehicle(externId : Long) : Boolean = {
    getInstance.removeVehicle(externId)
  }

  /**
    * @see (class VehicleMasterList { ... }).shutdown
    */
  def shutdown : List[Vehicle] = {
    val records : List[Vehicle] = getInstance.shutdown
    instance = null
    records
  }
}
