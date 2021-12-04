package compress.lz

import compress.CustomException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class LZ78Spec extends AnyFlatSpec with Matchers
  {
    behavior of "compressor.compress.lz.LZ78"
     it must "exist" in
       { "val compressor = LZ78" must compile }
     it must "contain methods `compress` and `uncompress` with correct signature" in
       { """val compressor = LZ78
           |val z : Seq[(Int, Char)]  = compressor.compress(Seq.empty[Char])
           |val s : Option[Seq[Char]] = compressor.uncompress(z)""".stripMargin must compile }

      it should "pass compress() exemple belle echelle" in {
          val l = LZ78
          val msg = "belle echelle"
          assert(l.compress(msg) ===  Seq((0,'b'), (0,'e'), (0,'l'), (3,'e'),
              (0,' '), (2,'c'), (0, 'h'), (2,'l'), (4,' '), (0,'!')))
      }
      it should "pass compress() uncompress() belle echelle" in {
          val l = LZ78
          val msg = "belle echelle"
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }
      /*
      it should "pass compress() uncompress() exemple  barbapapa" in {
          val msg = "barbapapa"
          val l = new LZW()
          assert(l.compress(msg) === Seq(98, 97, 114, 256, 112, 97, 260))
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }
      it should "pass compress() and uncompress() exemple  ababbabcababba" in {
          //https://www.youtube.com/watch?v=9gkiphD-VIY&t=236s
          val msg = "ababbabcababba"
          val l = new LZW(IndexedSeq("a", "b", "c"))
          val res = l.compress(msg).map(_ + 1)
          print(res)
          assert(res ===  Seq(1,2,4,5,2,3,4,6, 1))
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }
      it should "pass compress() uncompress() exemple  bbbabbaabbbb" in {
          val msg = "bbbabbaabbbb"
          val l = new LZW(IndexedSeq("a", "b"))
          assert(l.compress(msg) ===  Seq(1, 2, 0, 3, 4, 2, 1))
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }
      it should "pass  compress() uncompress() exemple ABRACADABRARABARABARA" in {
          val msg = "ABRACADABRARABARABARA"
          val l = new LZW(IndexedSeq("A", "B","C","D","R"))
          assert(l.compress(msg) ===  Seq(0,1,4,0,2,0,3,5,7,7,1,0,14,16,0))
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }
      it should "pass compress() uncompress() exemple  BASILE BAVE" in {
          val msg = "BASILE BAVE"
          val l = new LZW()
          assert(l.compress(msg) ===  Seq(66, 65, 83, 73, 76, 69, 32, 256, 86, 69))
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }
      it should "pass compress() uncompress() exemple  'banana bandana'" in {
          val msg = "banana bandana"
          val l = new LZW(IndexedSeq("a", "b", "d","n"," "))
          assert(l.compress(msg) ===  Seq(1,0,3,6,0,4,5,3,2,8))
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }
      it should "pass compress() exemple TWEET TWEET" in {
          val msg = "TWEET TWEET"
          val l = new LZW(IndexedSeq("E", "T", "W"," "))
          assert(l.compress(msg) ===  Seq(1,2,0,0,1,3,4,6,1))
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }

      it should "pass  compress()  uncompress() exemple  TWEET TWEET TWEET" in {
          val msg = "TWEET TWEET TWEET"
          val l = new LZW(IndexedSeq("E", "T", "W"," "))
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }

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

      it should "pass compress() uncompress() exemple vide" in {

          val msg = ""
          val l = new LZW()
          assert(l.uncompress(l.compress(msg)) == None)
      }*/

  }

