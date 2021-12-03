package compress


import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class RLESpec extends AnyFlatSpec with Matchers
  {
    behavior of "compress.RLE"
     it must "exist" in
       { "val compressor : RLE[Char] = new RLE[Char]" must compile }
     it must "contains methods `compress` and `uncompress` with correct signature" in
       { """val compressor : RLE[Char] = new RLE[Char]
           |val z : Seq[(Char, Int)]  = compressor.compress("")
           |val s : Option[Seq[Char]] = compressor.uncompress(z)""".stripMargin must compile }

      it should "exemple vide" in {
          val r = new RLE[Char]()
          val msg_original = ""
          val msg_compressed = r.compress(msg_original)
          val msg_uncompressed = r.uncompress(msg_compressed).get
          assert(msg_compressed === List( ))
          assert(msg_uncompressed === msg_original.toList)
      }
      it should "exemple a" in {
         val r = new RLE[Char]()
         val msg_original = "a"
         val msg_compressed = r.compress(msg_original)
         val msg_uncompressed = r.uncompress(msg_compressed).get
         assert(msg_compressed === List(('a',1)))
          assert(msg_uncompressed === msg_original.toList)
     }
      it should "exemple aaaaabbcbbb" in {
          val r = new RLE[Char]()
          val msg_original = "aaaaabbcbbb"
          val msg_compressed = r.compress(msg_original)
          val msg_uncompressed = r.uncompress(msg_compressed).get
          assert(msg_compressed === List( ('a',5), ('b',2), ('c',1), ('b',3) ))
          assert(msg_uncompressed === msg_original.toList)
      }
      it should "exemple Seq(1, 1, 0, 0, 0, 0, 0, 22, 2, 2, 2, 2, 2, 2)" in {
          val r = new RLE[Int]()
          val msg_original = Seq(1, 1, 0, 0, 0, 0, 0, 22, 2, 2, 2, 2, 2, 2)
          val msg_compressed = r.compress(msg_original)
          val msg_uncompressed = r.uncompress(msg_compressed).get
          assert(msg_compressed === List( (1,2), (0,5), (22,1), (2,6) ))
          assert(msg_uncompressed === msg_original.toList)
      }
      it should "exemple REELLEMENT" in {
          val r = new RLE[Char]()
          val msg_original = "REELLEMENT"
          val msg_compressed = r.compress(msg_original)
          val msg_uncompressed = r.uncompress(msg_compressed).get
          assert(msg_compressed === List( ('R',1), ('E',2), ('L',2), ('E',1), ('M',1), ('E',1), ('N',1), ('T',1) ) )
          assert(msg_uncompressed === msg_original.toList)
      }


  }
