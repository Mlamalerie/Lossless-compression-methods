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

      it should "pass compress() exemple belle echelle " in {
          val l = LZ78
          val msg = "belle echelle !"
          assert(l.compress(msg) ===  Seq((0,'b'), (0,'e'), (0,'l'), (3,'e'),
              (0,' '), (2,'c'), (0, 'h'), (2,'l'), (4,' '), (0,'!')))
      }
      it should "pass compress() uncompress() belle echelle !" in {
          val l = LZ78
          val msg = "belle echelle !"
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }
      it should "pass compress() exemple abxacadabxa" in {
          val l = LZ78
          val msg = "abxacadabxa"
          assert(l.compress(msg) === Seq((0,'a'), (0,'b'), (0,'x'), (1,'c'), (1,'d'), (1,'b'), (3,'a')))
      }

      it should "pass compress() exemple bbbabbaabbbb" in {
          val l = LZ78
          val msg = "bbbabbaabbbb"
          assert(l.compress(msg) === Seq((0, 'b'), (1, 'b'), (0, 'a'), (2, 'a'), (3, 'b'), (2, 'b')))
      }


      it should "pass compress() exemple barbapapa" in {
          val l = LZ78
          val msg = "bbbabbaabbbb"
          assert(l.compress(msg) === Seq((0, 'b'), (1, 'b'), (0, 'a'), (2, 'a'), (3, 'b'), (2, 'b')))
      }


      it should "pass uncompress() exemple Seq((0, 'b'), (0, 'a'), (2, 'a'), (3, 'b'), (4, 'a'))" in {
          val l = LZ78
          val msg = Seq((0, 'b'), (0, 'a'), (2, 'a'), (3, 'b'), (4, 'a'))
          assert(l.uncompress(msg) === Some("baaaaabaaba".toList) )
      }


      it should "pass compress() uncompress() exemple  barbapapa" in {
          val msg = "barbapapa"
          val l = LZ78
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }

      it should "pass  compress() uncompress() exemple ABRACADABRARABARABARA" in {
          val msg = "ABRACADABRARABARABARA"
          val l = LZ78
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }
      it should "pass compress() uncompress() exemple  BASILE BAVE" in {
          val msg = "BASILE BAVE"
          val l = LZ78
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }
      it should "pass compress() uncompress() exemple  'banana bandana'" in {
          val msg = "banana bandana"
          val l = LZ78
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }
      it should "pass compress() exemple TWEET TWEET" in {
          val msg = "TWEET TWEET"
          val l = LZ78
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }

      it should "pass  compress()  uncompress() exemple  TWEET TWEET TWEET" in {
          val msg = "TWEET TWEET TWEET"
          val l = new LZW(IndexedSeq("E", "T", "W"," "))
          assert(l.uncompress(l.compress(msg)) == Some(msg.toList))
      }

      it should "pass compress() uncompress() exemple vide" in {
          val msg = ""
          val l = LZ78
          assert(l.uncompress(l.compress(msg)) == None)
      }

  }

