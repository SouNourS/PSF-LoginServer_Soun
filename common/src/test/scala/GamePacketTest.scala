// Copyright (c) 2016 PSForever.net to present
import java.net.{InetAddress, InetSocketAddress}
import java.util.Locale

import org.specs2.mutable._
import net.psforever.packet._
import net.psforever.packet.game._
import net.psforever.packet.game.objectcreate.{InventoryItem, _}
import net.psforever.types._
import scodec.{Attempt, Err}
import scodec.Attempt.Successful
import scodec.bits._

class GamePacketTest extends Specification {

  Locale.setDefault(Locale.US);

  "PlanetSide game packet" in {

    "ConnectToWorldRequestMessage" should {
      val string = hex"03 8667656D696E69 0000000000000000 00000000 00000000 00000000 00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  80 00 00 "

      "decode" in {
        PacketCoding.DecodePacket(string).require match {
          case ConnectToWorldRequestMessage(serverName, token, majorVersion, minorVersion, revision, buildDate, unk) =>
            serverName mustEqual "gemini"
            token mustEqual ""
            majorVersion mustEqual 0
            minorVersion mustEqual 0
            revision mustEqual 0
            buildDate mustEqual ""
            unk mustEqual 0
          case default =>
            ko
        }
      }

      "encode" in {
        val msg = ConnectToWorldRequestMessage("gemini", "", 0, 0, 0, "", 0)
        val pkt = PacketCoding.EncodePacket(msg).require.toByteVector

        pkt mustEqual string
      }
    }

    }
  }
