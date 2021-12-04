package compress.lz

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import Dictionaries._
import compress.CustomException

class LZWSpec extends AnyFlatSpec with Matchers
  {
    behavior of "compressor.compress.lz.LZW"
     it must "exist" in
       { "val compressor : LZW = new LZW()" must compile }
     it must "contain methods `compress` and `uncompress` with correct signature" in
       { """val compressor : LZW  = new LZW()
           |val z : Seq[Int]  = compressor.compress(Seq.empty[Char])
           |val s : Option[Seq[Char]] = compressor.uncompress(z)""".stripMargin must compile }
     it must "contain field `initialDictionary` with correct type" in
       { """val compressor : LZW  = new LZW()
           |val dict : Dictionary = compressor.initialDictionary""".stripMargin must compile }


      it should "pass exemple compress() belle echelle" in {
          val l = new LZW()
          val msg = "belle echelle"
         assert(l.compress(msg) ===  Seq(98, 101, 108, 108, 101, 32, 101, 99, 104, 257, 259))

      }
      it should "pass exemple compress() ababbabcababba" in {
          //https://www.youtube.com/watch?v=9gkiphD-VIY&t=236s
          val msg = "ababbabcababba"
          val l = new LZW(IndexedSeq("a", "b", "c"))
          val res = l.compress(msg).map(_ + 1)
          print(res)
          assert(res ===  Seq(1,2,4,5,2,3,4,6, 1))
      }
      it should "pass exemple compress() bbbabbaabbbb" in {
          val msg = "bbbabbaabbbb"
          val l = new LZW(IndexedSeq("a", "b"))
          assert(l.compress(msg) ===  Seq(1, 2, 0, 3, 4, 2, 1))
      }
      it should "pass exemple compress() ABRACADABRARABARABARA" in {
          val msg = "ABRACADABRARABARABARA"
          val l = new LZW(IndexedSeq("A", "B","C","D","R"))
          assert(l.compress(msg) ===  Seq(0,1,4,0,2,0,3,5,7,7,1,0,14,16,0))
      }
      it should "pass exemple compress() BASILE BAVE" in {
          val msg = "BASILE BAVE"
          val l = new LZW()
          assert(l.compress(msg) ===  Seq(66, 65, 83, 73, 76, 69, 32, 256, 86, 69))
      }
      it should "pass exemple compress() 'banana bandana'" in {
          val msg = "banana bandana"
          val l = new LZW(IndexedSeq("a", "b", "d","n"," "))
          assert(l.compress(msg) ===  Seq(1,0,3,6,0,4,5,3,2,8))
      }
      it should "pass exemple compress() TWEET TWEET" in {
          val msg = "TWEET TWEET"
          val l = new LZW(IndexedSeq("E", "T", "W"," "))
          assert(l.compress(msg) ===  Seq(1,2,0,0,1,3,4,6,1))
      }
      /*
      it should "pass exemple compress() TWEET TWEET TWEET" in {
          val msg = "TWEET TWEET TWEET"
          val l = new LZW(IndexedSeq("E", "T", "W"," "))
          assert(l.compress(msg) ===  Seq(1,2,0,0,1,3,4,6,8,10,1)) // j'ai tester à la main je trouve Seq(1,2,0,0,1,3,4,6,8,10,1)
          // desactiver ça psk je suis pas sure
      }*/

      it should "dont pass exemple compress() 'banana_bandana'" in {
          val msg = "banana_bandana"
          val lz = new LZW(IndexedSeq("a", "b", "d","n"," "))
          assert({try {
              lz.compress(msg)
          }
          catch {
              case c: CustomException => true
              case _ => false
          }} === true)
      }
/*
      it should "pass exemple uncompress() Seq(1,2,0,4,1)" in {
          val l = new LZW(IndexedSeq("a", "b"))
          val msg_compressed = Seq(1,2,0,4,1)
          assert(l.uncompress(msg_compressed) ===  Some("bbbaaab".toList))

      }
      it should "pass exemple uncompress() Seq(2,0,4,6,5,8)" in {
          val l = new LZW(IndexedSeq("A", "C","H","S"))
          val msg_compressed = Seq(2,0,4,6,5,8)
          assert(l.uncompress(msg_compressed) ===  Some("HA".toList))

      }*/
  }
