/**
  * Created by SouNourS on 20/12/2016.
  */

import net.psforever.packet._
import scodec.bits._

import scala.io.Source
import java.io.FileWriter

import util.control.Breaks._
import java.io.File

import net.psforever.packet.control._
import scodec.Attempt
import scodec.Attempt.{Failure, Successful}

object Xtools {

  def main(args: Array[String]): Unit = {

    // TODO mise en forme
//      val FileToRead = "D:\\decode_capture2\\tata.txt"
//      val FileToWrite = "D:\\decode_capture2\\tata2.txt"
//      val fw = new FileWriter(FileToWrite, true)
//
//
//      for (line <- Source.fromFile(FileToRead).getLines()) {
//
//        val isline = line.indexOf("=")
//
//        if (isline != -1) {
//          fw.write(line.dropRight(line.length - isline + 1) + "," +  System.getProperty("line.separator"))
//        }
//        else {
//          fw.write("NoAmmo_" + line + "," + System.getProperty("line.separator"))
//        }
//      }
//      fw.close()


    // TODO find IP in log
//    for (file <- new File("F:\\tmp\\psf_logs\\").listFiles) {
//      val FileToRead = file.toString
//      val FileToWrite = "F:\\tmp\\psf_logs\\iplist.txt"
//      val fw = new FileWriter(FileToWrite, true)
//
//
//      for (line <- Source.fromFile(FileToRead).getLines()) {
//
//        val isline = line.indexOf(" CharacterCreateRequestMessage(")
//        val isline2 = line.indexOf(" world-session-router - New session ID=")
//        val isline3 = line.indexOf(" CharacterRequestMessage(")
//        //        println(isline)
//        //        println(isline2)
//        if (isline != -1 || isline2 != -1 || isline3 != -1)
//          fw.write(line + System.getProperty("line.separator"))
//      }
//      fw.close()
//    }

      // TODO decode packet
//    for (file <- new File("D:\\decode_capture\\").listFiles) {
    for (file <- new File("D:\\decode_capture\\").listFiles) {
      val FileToRead = file.toString
      val FileToWrite = "D:\\_" + FileToRead.drop(3).dropRight(FileToRead.toString.length - FileToRead.toString.indexOf(".txt")) + "_decoded.txt"
      val fw = new FileWriter(FileToWrite, true)

      for (line <- Source.fromFile(FileToRead).getLines()) {

        val linetest = line substring(0, 2)
        val linetest2 = line substring(0, 17)
        if (linetest != "IF") {

////          println(ByteVector.fromValidHex(line.drop(line.lastIndexOf(' '))))
////          println(PacketCoding.DecodePacket(ByteVector.fromValidHex(line.drop(line.lastIndexOf(' ')))))
//          handlePkt(PacketCoding.DecodePacket(ByteVector.fromValidHex(line.drop(line.lastIndexOf(' ')))))

          fw.write(System.getProperty("line.separator") + "#" + line + System.getProperty("line.separator"))
          var isSlotted = -1
          var isMultiPacketEx = -1
          var isMultiPacket = -1
          var isMultiPacketExSlot = -1
          var isHandleGamePacket = -1
          var AfterDecode = Fdecode(line.drop(line.lastIndexOf(' ')))
          var AfterDecode2 = ""
          var AfterDecode3 = ""
          var AfterDecode4 = ""
          var AfterDecode5 = ""

          isMultiPacket = AfterDecode.indexOf("Successful(MultiPacket(")
          isSlotted = AfterDecode.indexOf("Successful(SlottedMetaPacket(")
          isMultiPacketEx = AfterDecode.indexOf("Successful(MultiPacketEx(")

          if (isSlotted != 0 && isMultiPacket == -1 && isMultiPacketEx == -1) {
            fw.write(AfterDecode + System.getProperty("line.separator"))
            //        println(AfterDecode )
          }

          if (isMultiPacket != -1) {
            fw.write(AfterDecode + System.getProperty("line.separator"))
            //        println(AfterDecode)
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
                //            println("> " + AfterDecode2)
                AfterDecode3 = Fdecode(AfterDecode2.drop(AfterDecode2.lastIndexOf(" 0x") + 3).dropRight(AfterDecode2.length - AfterDecode2.indexOf(")")))
                isMultiPacketExSlot = AfterDecode3.indexOf("Successful(MultiPacketEx(")
                if (isMultiPacketExSlot != -1) {
                  fw.write("-> " + AfterDecode3 + System.getProperty("line.separator"))
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
                    //                println("--> " + AfterDecode4 )
                  }
                  isMultiPacketEx = -1
                  isMultiPacketExSlot = -1
                } else {
                  fw.write("-> " + AfterDecode3 + System.getProperty("line.separator"))
                  //                println("-> " + AfterDecode3 )
                }
              } else {
                fw.write("> " + AfterDecode2 + System.getProperty("line.separator"))
                //              println("> " + AfterDecode2 )
              }
            }
          }
          if (isSlotted == 0 && isMultiPacket == -1) {
            fw.write(AfterDecode + System.getProperty("line.separator"))
            //        println(AfterDecode)
            AfterDecode = Fdecode(AfterDecode.drop(AfterDecode.lastIndexOf(" 0x") + 3).dropRight(AfterDecode.length - AfterDecode.indexOf(")")))
            isMultiPacketExSlot = AfterDecode.indexOf("Successful(MultiPacketEx(")
            isHandleGamePacket = AfterDecode.indexOf("Successful(HandleGamePacket(")
            if (isHandleGamePacket != -1) {
              fw.write("> " + AfterDecode + System.getProperty("line.separator"))
//              println("> " + AfterDecode )
              if(AfterDecode.lastIndexOf(" 0x") != -1){
              AfterDecode5 = Fdecode(AfterDecode.drop(AfterDecode.lastIndexOf(" 0x") + 3).dropRight(AfterDecode.length - AfterDecode.indexOf(")")))
              fw.write("-> " + AfterDecode5 + System.getProperty("line.separator"))
//              println("-> " + AfterDecode5 )
              }
            }
            if (isMultiPacketExSlot == -1 && isHandleGamePacket == -1) {
              fw.write("> " + AfterDecode + System.getProperty("line.separator"))
              //          println("> " + AfterDecode )
            }
            if (isMultiPacketExSlot != -1 && isHandleGamePacket == -1) {
              fw.write("> " + AfterDecode + System.getProperty("line.separator"))
              //          println("> " + AfterDecode )
              var xindex3 = 1
              var zindex3 = 0
              var boucle3 = 0
              while (boucle3 != -1) {
                AfterDecode2 = Fdecode(AfterDecode.drop(AfterDecode.indexOf(" 0x", xindex3) + 3).dropRight(AfterDecode.length - AfterDecode.indexOf(")", zindex3 + 1)))
                fw.write("-> " + AfterDecode2 + System.getProperty("line.separator"))
                //            println("-> " + AfterDecode2)
                xindex3 = AfterDecode.indexOf(" 0x", xindex3) + 1
                boucle3 = AfterDecode.indexOf(" 0x", xindex3)
                zindex3 = AfterDecode.indexOf(")", zindex3) + 1
              }
            }
          }
          if ((isMultiPacketEx != -1 || isMultiPacketExSlot != -1) && isSlotted != 0) {
            fw.write(AfterDecode + System.getProperty("line.separator"))
            //        println( AfterDecode )
            var xindex = 1
            var zindex = 0
            var boucle = 0
            while (boucle != -1) {
              AfterDecode2 = Fdecode(AfterDecode.drop(AfterDecode.indexOf(" 0x", xindex) + 3).dropRight(AfterDecode.length - AfterDecode.indexOf(")", zindex + 1)))
              fw.write("> " + AfterDecode2 + System.getProperty("line.separator"))
              //          println("> " + AfterDecode2)
              xindex = AfterDecode.indexOf(" 0x", xindex) + 1
              boucle = AfterDecode.indexOf(" 0x", xindex)
              zindex = AfterDecode.indexOf(")", zindex) + 1
            }
          }
        }
      }
      fw.close()
    }





      // TODO : end
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