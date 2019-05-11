/**
  * Created by SouNourS on 20/12/2016.
  */

// Make sure the input files have UTF8 encoding!

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.charset.CodingErrorAction

import net.psforever.packet._
import scodec.Attempt
import scodec.bits._

import scala.io.{Codec, Source}

object Xtoolspar {

  def main(args: Array[String]): Unit = {
    val dirToProcess = "D:/decode_capture"
    val dirForDecoded = "D:/_decode_capture"
    val tempDir = "C:/temp"


    val files = new File(dirToProcess).listFiles

    // TODO decode packet
    files.par.foreach { f =>
      var version : Int = 0
      val file = new File(f.toString)
      val fileOut : String = file.getName().dropRight(file.getName().length - file.getName().indexOf(".txt"))
      var FileToWrite = tempDir + "/" + fileOut + "_" + version + ".txt"
      val FileToMoveTo = dirForDecoded + "/" + fileOut + "_" + version + ".txt"

      if (new File(FileToMoveTo).exists()) {
        println(s"File ${fileOut}_$version.txt exists - skipping")
      } else {
        println(s"${FileToMoveTo} doesn't exist - Got new file ${file.getName}")


        val FileToRead = file.toString
        var fw = new BufferedWriter(new FileWriter(FileToWrite, false))
        var index : Int = 0
        var indexLine : Int = 0

//        try {
          val decoder = Codec.UTF8.decoder.onMalformedInput(CodingErrorAction.IGNORE)
          for (line <- Source.fromFile(FileToRead)(decoder).getLines()) {
            indexLine += 1
            val lineTest: String = line.substring(0, 2)
            if (!lineTest.equalsIgnoreCase("IF")) {
              ////          println(ByteVector.fromValidHex(line.drop(line.lastIndexOf(' '))))
              ////          println(PacketCoding.DecodePacket(ByteVector.fromValidHex(line.drop(line.lastIndexOf(' ')))))
              //          handlePkt(PacketCoding.DecodePacket(ByteVector.fromValidHex(line.drop(line.lastIndexOf(' ')))))

              fw.write(System.getProperty("line.separator") + "#" + line + System.getProperty("line.separator"))
              index += 1
              var isSlotted = -1
              var isMultiPacketEx = -1
              var isMultiPacket = -1
              var isMultiPacketExSlot = -1
              var isHandleGamePacket = -1
              val decodedLine = line.drop(line.lastIndexOf(' '))
              var AfterDecode = Fdecode(decodedLine)
//              println(AfterDecode)
              var AfterDecode2 = ""
              var AfterDecode3 = ""
              var AfterDecode4 = ""
              var AfterDecode5 = ""

              isMultiPacket = AfterDecode.indexOf("Successful(MultiPacket(")
              isSlotted = AfterDecode.indexOf("Successful(SlottedMetaPacket(")
              isMultiPacketEx = AfterDecode.indexOf("Successful(MultiPacketEx(")

              if (isSlotted != 0 && isMultiPacket == -1 && isMultiPacketEx == -1) {
                fw.write(AfterDecode + System.getProperty("line.separator"))
                index += 1
//                        println(AfterDecode )
              }

              if (isMultiPacket != -1) {
                fw.write(AfterDecode + System.getProperty("line.separator"))
                index += 1
//                        println(AfterDecode)
                var xindex1 = 1
                var zindex1 = 0
                var boucle1 = 0
                while (boucle1 != -1) {
                  AfterDecode2 = Fdecode(AfterDecode.drop(AfterDecode.indexOf(" 0x", xindex1) + 3).dropRight(AfterDecode.length - AfterDecode.indexOf(")", zindex1 + 1)))
                  xindex1 = AfterDecode.indexOf(" 0x", xindex1) + 1
                  boucle1 = AfterDecode.indexOf(" 0x", xindex1)
                  zindex1 = AfterDecode.indexOf(")", zindex1) + 1
                  isSlotted = AfterDecode2.indexOf("Successful(SlottedMetaPacket(")
                  if (isSlotted == 0) {
                    fw.write("> " + AfterDecode2 + System.getProperty("line.separator"))
                    index += 1
                    //            println("> " + AfterDecode2)
                    AfterDecode3 = Fdecode(AfterDecode2.drop(AfterDecode2.lastIndexOf(" 0x") + 3).dropRight(AfterDecode2.length - AfterDecode2.indexOf(")")))
                    isMultiPacketExSlot = AfterDecode3.indexOf("Successful(MultiPacketEx(")
                    if (isMultiPacketExSlot != -1) {
                      fw.write("-> " + AfterDecode3 + System.getProperty("line.separator"))
                      index += 1
                      //                println("-> " + AfterDecode3)
                      var xindex2 = 1
                      var zindex2 = 0
                      var boucle2 = 0
                      while (boucle2 != -1) {
                        AfterDecode4 = Fdecode(AfterDecode3.drop(AfterDecode3.indexOf(" 0x", xindex2) + 3).dropRight(AfterDecode3.length - AfterDecode3.indexOf(")", zindex2 + 1)))
                        xindex2 = AfterDecode3.indexOf(" 0x", xindex2) + 1
                        boucle2 = AfterDecode3.indexOf(" 0x", xindex2)
                        zindex2 = AfterDecode3.indexOf(")", zindex2) + 1
                        fw.write("--> " + AfterDecode4 + System.getProperty("line.separator"))
                        index += 1
                        //                println("--> " + AfterDecode4 )
                      }
                      isMultiPacketEx = -1
                      isMultiPacketExSlot = -1
                    } else {
                      fw.write("-> " + AfterDecode3 + System.getProperty("line.separator"))
                      index += 1
                      //                println("-> " + AfterDecode3 )
                    }
                  } else {
                    fw.write("> " + AfterDecode2 + System.getProperty("line.separator"))
                    index += 1
                    //              println("> " + AfterDecode2 )
                  }
                }
              }
              if (isSlotted == 0 && isMultiPacket == -1) {
                fw.write(AfterDecode + System.getProperty("line.separator"))
                index += 1
//                        println(AfterDecode)
                AfterDecode = Fdecode(AfterDecode.drop(AfterDecode.lastIndexOf(" 0x") + 3).dropRight(AfterDecode.length - AfterDecode.indexOf(")")))
                isMultiPacketExSlot = AfterDecode.indexOf("Successful(MultiPacketEx(")
                isHandleGamePacket = AfterDecode.indexOf("Successful(HandleGamePacket(")
                if (isHandleGamePacket != -1) {
                  fw.write("> " + AfterDecode + System.getProperty("line.separator"))
                  index += 1
                  //              println("> " + AfterDecode )
                  if (AfterDecode.lastIndexOf(" 0x") != -1) {
                    AfterDecode5 = Fdecode(AfterDecode.drop(AfterDecode.lastIndexOf(" 0x") + 3).dropRight(AfterDecode.length - AfterDecode.indexOf(")")))
                    fw.write("-> " + AfterDecode5 + System.getProperty("line.separator"))
                    index += 1
                    //              println("-> " + AfterDecode5 )
                  }
                }
                if (isMultiPacketExSlot == -1 && isHandleGamePacket == -1) {
                  fw.write("> " + AfterDecode + System.getProperty("line.separator"))
                  index += 1
                  //          println("> " + AfterDecode )
                }
                if (isMultiPacketExSlot != -1 && isHandleGamePacket == -1) {
                  fw.write("> " + AfterDecode + System.getProperty("line.separator"))
                  index += 1
                  //          println("> " + AfterDecode )
                  var xindex3 = 1
                  var zindex3 = 0
                  var boucle3 = 0
                  while (boucle3 != -1) {
                    AfterDecode2 = Fdecode(AfterDecode.drop(AfterDecode.indexOf(" 0x", xindex3) + 3).dropRight(AfterDecode.length - AfterDecode.indexOf(")", zindex3 + 1)))
                    fw.write("-> " + AfterDecode2 + System.getProperty("line.separator"))
                    index += 1
                    //            println("-> " + AfterDecode2)
                    xindex3 = AfterDecode.indexOf(" 0x", xindex3) + 1
                    boucle3 = AfterDecode.indexOf(" 0x", xindex3)
                    zindex3 = AfterDecode.indexOf(")", zindex3) + 1
                  }
                }
              }
              if ((isMultiPacketEx != -1 || isMultiPacketExSlot != -1) && isSlotted != 0) {
                fw.write(AfterDecode + System.getProperty("line.separator"))
                index += 1
                //        println( AfterDecode )
                var xindex = 1
                var zindex = 0
                var boucle = 0
                while (boucle != -1) {
                  AfterDecode2 = Fdecode(AfterDecode.drop(AfterDecode.indexOf(" 0x", xindex) + 3).dropRight(AfterDecode.length - AfterDecode.indexOf(")", zindex + 1)))
                  fw.write("> " + AfterDecode2 + System.getProperty("line.separator"))
                  index += 1
                  //          println("> " + AfterDecode2)
                  xindex = AfterDecode.indexOf(" 0x", xindex) + 1
                  boucle = AfterDecode.indexOf(" 0x", xindex)
                  zindex = AfterDecode.indexOf(")", zindex) + 1
                }
              }
              println(index, indexLine, fileOut)
              if (index > 2000000) {
                fw.close()
                moveFile(FileToWrite, FileToMoveTo)
                index = 0
                version += 1
                FileToWrite = tempDir + "/" + fileOut + "_" + version + ".txt"
                fw = new BufferedWriter(new FileWriter(FileToWrite, false))
              }
            }
          }
//        }
//        catch {
//          case _: Throwable =>
//          println(s"File ${file.getName} threw an exception")
//        }
//        finally {
          fw.close()
          moveFile(FileToWrite, FileToMoveTo)
//        }
      }
    }




    // TODO : end
  }

  import java.nio.file.{Files, Paths, StandardCopyOption}

  def moveFile(sourcePath: String, targetPath: String): Boolean = {
    var flag = true
    try
      Files.move(Paths.get(sourcePath), Paths.get(targetPath), StandardCopyOption.REPLACE_EXISTING)
    catch {
      case e: Exception =>
        flag = false
        e.printStackTrace()
    }
    flag
  }

  def Fdecode(toto: String): String = {
    val ADecode = PacketCoding.DecodePacket(ByteVector.fromValidHex(toto)).toString;
    return ADecode
  }


  def handlePkt(pkt : Attempt[PlanetSidePacket]) : Unit = pkt match {
    case ctrl : PlanetSideControlPacket =>
      println(ctrl)
    //      handleControlPkt(ctrl)
    case game : PlanetSideGamePacket =>
      println(game)
    //      handleGamePkt(game)
    case default => println(s"Invalid packet class received: $default")
  }

  def handlePktContainer(pkt : PlanetSidePacketContainer) : Unit = pkt match {
    case ctrl @ ControlPacket(opcode, ctrlPkt) =>
      //      println(pkt)
      println(ctrlPkt)
    //      handleControlPkt(ctrlPkt)
    case game @ GamePacket(opcode, seq, gamePkt) =>
      //      println(pkt)
      println(gamePkt)
    //      handleGamePkt(gamePkt)
    case default => println(s"Invalid packet container class received: $default")
  }

  //  def handleControlPkt(pkt : PlanetSideControlPacket) = {
  //    //    println(pkt)
  //    pkt match {
  //      case SlottedMetaPacket(slot, subslot, innerPacket) =>
  ////        sendResponse(PacketCoding.CreateControlPacket(SlottedMetaAck(slot, subslot)))
  //
  //        PacketCoding.DecodePacket(innerPacket) match {
  //          case Failure(e) =>
  //            println(innerPacket.toString)
  //            println(s"Failed to decode inner packet of SlottedMetaPacket: $e")
  //          case Successful(v) =>
  //            handlePkt(v)
  //        }
  //      case sync @ ControlSync(diff, unk, f1, f2, f3, f4, fa, fb) =>
  //        println(s"SYNC: ${sync}")
  //        val serverTick = Math.abs(System.nanoTime().toInt) // limit the size to prevent encoding error
  ////        sendResponse(PacketCoding.CreateControlPacket(ControlSyncResp(diff, serverTick, fa, fb, fb, fa)))
  //      case MultiPacket(packets) =>
  //        packets.foreach { pkt =>
  //          PacketCoding.DecodePacket(pkt) match {
  //            case Failure(e) =>
  //              println(pkt.toString)
  //              println(s"Failed to decode inner packet of MultiPacket: $e")
  //            case Successful(v) =>
  //              handlePkt(v)
  //          }
  //        }
  //      case MultiPacketEx(packets) =>
  //        packets.foreach { pkt =>
  //          PacketCoding.DecodePacket(pkt) match {
  //            case Failure(e) =>
  //              println(pkt.toString)
  //              println(s"Failed to decode inner packet of MultiPacketEx: $e")
  //            case Successful(v) =>
  //              handlePkt(v)
  //          }
  //        }
  //      case default =>
  //        println(s"Unhandled ControlPacket $default")
  //    }
  //  }

}