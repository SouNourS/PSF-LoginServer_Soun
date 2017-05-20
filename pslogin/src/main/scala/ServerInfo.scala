// Copyright (c) 2017 PSForever


/**
  * TEST CLASS
  * A class for storing server informations
  * TEST CLASS
  */
final class ServerInfo {

  val started: Long = System.currentTimeMillis

  var test : Long = 0


  def mapRotation(time: Long): Long = {
    println(test)
    println(time)
    val tata : Long = time - test
    if (tata > 10000) {
      test = time
    }
    test
  }

  var log : Boolean = true


}


object ServerInfo {
  private var instance: Option[ServerInfo] = None

  private def getInstance: ServerInfo = {
    if (instance.isEmpty)
      instance = Option(new ServerInfo)
    instance.get
  }

  def mapRotation(time: Long): Long = {
    getInstance.mapRotation(time)
  }

  def getLog : Boolean = {
    getInstance.log
  }
  def setLog(newLogValue : Boolean) : Unit = {
    getInstance.log = newLogValue
  }


}
