/**
  * Created by SouNourS on 20/12/2016.
  */

import net.psforever.packet._
import scodec.bits._
import scala.io.Source
import java.io.FileWriter
import java.io.File

object Xtools {
  val FileToRead = "D:\\all-captures-07-13-16\\PSCap-2016-06-19_09-38-17-.txt"
  val FileToWrite = "D:\\all-captures-07-13-16\\PSCap-2016-06-19_09-38-17-_decoded.txt"
  val fw = new FileWriter(FileToWrite, true)

  def main(args: Array[String]): Unit = {

    for (line <- Source.fromFile(FileToRead).getLines()) {
      fw.write("!" + line + System.getProperty("line.separator"))
      var isSlotted = -1
      var isMultiPacketEx = -1
      var isMultiPacketExSlot = -1
      var string = ByteVector.fromValidHex(line.drop(line.lastIndexOf(' ')))
      var AfterDecode = PacketCoding.DecodePacket(string).toString
      var AfterDecode2 = ""

      isSlotted = AfterDecode.indexOf("Successful(SlottedMetaPacket(")
      isMultiPacketEx = AfterDecode.indexOf("Successful(MultiPacketEx(")

      if( isSlotted != 0 && isMultiPacketEx == -1){
        fw.write("_ " + AfterDecode + System.getProperty("line.separator"))
      }

      if( isSlotted == 0 ){
        string = ByteVector.fromValidHex(AfterDecode.drop(AfterDecode.lastIndexOf(" 0x")+3).dropRight(AfterDecode.length - AfterDecode.indexOf(")")))
        AfterDecode = PacketCoding.DecodePacket(string).toString
        isMultiPacketExSlot = AfterDecode.indexOf("Successful(MultiPacketEx(")
        if( isMultiPacketExSlot == -1 ){
          fw.write("_ " + AfterDecode + System.getProperty("line.separator"))
        }
      }
      if( isMultiPacketEx != -1 || isMultiPacketExSlot != -1 ){
        for( a <- 0 to 10){
          if(AfterDecode.indexOf(" 0x", AfterDecode.indexOf(" 0x") + a) != AfterDecode.indexOf(" 0x", AfterDecode.indexOf(" 0x") + a-1) || a == 0){
            string = ByteVector.fromValidHex(AfterDecode.drop(AfterDecode.indexOf(" 0x", AfterDecode.indexOf(" 0x") + a)+3).dropRight(AfterDecode.length - AfterDecode.indexOf(")", AfterDecode.indexOf(")") + a)))
            AfterDecode2 = PacketCoding.DecodePacket(string).toString
            fw.write("_ " + AfterDecode2 + System.getProperty("line.separator"))
          }
        }
      }
    }
  }

}
