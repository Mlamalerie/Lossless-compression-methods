package compress.statistic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class HuffmanSpec extends AnyFlatSpec with Matchers
  {
    behavior of "compressor.compress.statistic.Huffman"
     it must "exist" in
       { "val compressor : Huffman[Char] = new Huffman(Seq.empty[Char])" must compile }
     it must "contain methods `compress` and `uncompress` with correct signature" in
       { """val compressor : Huffman[Char] = new Huffman(Seq.empty[Char])
           |val z : Seq[Bit]          = compressor.compress(Seq.empty[Char])
           |val s : Option[Seq[Char]] = compressor.uncompress(z)""".stripMargin must compile }
     it must "contain fields `entropy`, `occurrences`, `orderedCounts` and `meanLength` with correct type" in
       { """val compressor : Huffman[Char] = new Huffman(Seq.empty[Char])
           |val h : Double = compressor.entropy
           |val occs : Map[Char, Int] = compressor.occurrences
           |val o_occs : Seq[(Char, Int)] = compressor.orderedCounts
           |val t : Option[EncodingTree[Char]] = compressor.tree""".stripMargin must compile }


      it should "initiliazation good" in {
          val t = new Huffman[Char]("MLAMALI").tree
          assert(t !== None)
          val t2 = new Huffman[Char]("M").tree
          assert(t2 !== None)
          val t3 = new Huffman[Char]("").tree
          assert(t3 === None)

      }
      it should "val dict_correspondence good" in {
          val t = new Huffman[Char]("MLAMALI").tree.get

          assert(t.dict_correspondence('L') === Seq(Zero,Zero))
          assert(t.dict_correspondence('M') === Seq(Zero,One))

          // test arbre 2 nodes
          val t1 = new Huffman[Char]("AB").tree.get
          assert(t1.dict_correspondence('A').length === 1)
          assert(t1.dict_correspondence('B').length === 1)
          assert(t1.dict_correspondence('B') !== t1.dict_correspondence('A'))
          // test arbre 1 nodes
          val t2 = new Huffman[Char]("A").tree.get
          assert(t2.dict_correspondence('A').length === 1)


      }

      it should "val dict_correspondence must have unique values" in {
          val t = new Huffman[Char]("MLAMALI ARI MATHIEEUUU JOO REDOUAANE").tree.get
          val values = t.dict_correspondence.values.toList
          assert((values.map(x => values.count(_ != x)).sum == (values.length - 1)*values.length) === true)


          // test arbre 2 nodes
          val t1 = new Huffman[Char]("AB").tree.get
          val values1 = t.dict_correspondence.values.toList
          assert((values1.map(x => values.count(_ != x)).sum == (values1.length - 1)*values1.length) === true)


      }


      it should "encode() good" in {
          val t = new Huffman[Char]("MLAMALI").tree.get
          assert(t.encode('L') === Some(Seq(Zero,Zero)))
          assert(t.encode('M') === Some(Seq(Zero,One)))
          assert(t.encode('m') === None)
          assert(t.encode('x') === None)
      }

      it should "decodeOnce() good" in {
          val t = new Huffman[Char]("MLAMALI").tree.get
          assert(t.decodeOnce(t.dict_correspondence('M') ++ t.dict_correspondence('L') ++ t.dict_correspondence('A')) === Some(('M',t.dict_correspondence('L') ++ t.dict_correspondence('A'))))
          assert(t.decodeOnce(t.dict_correspondence('A') ++ Seq(Zero,Zero,One,Zero)) === Some(('A',Seq(Zero,Zero,One,Zero))))
          assert(t.decodeOnce(t.dict_correspondence('I') ++ Seq(One)) === Some(('I',Seq(One))))
          assert(t.decodeOnce(Seq(Zero)) === None)
          assert(t.decodeOnce(Seq()) === None)

      }

      it should "decode() good" in {
          val t = new Huffman[Char]("MLAMALI").tree.get
          assert(t.decode(t.dict_correspondence('I') ++ t.dict_correspondence('I') ) === Some("II".toList))
          assert(t.decode(t.dict_correspondence('M') ++ t.dict_correspondence('L')  ) === Some("ML".toList))
          assert(t.decode(t.dict_correspondence('M') ) === Some("M".toList))
          assert(t.decode(Seq(Zero, One, Zero,One,One,Zero,Zero)) === None)
          assert(t.decode(Seq(Zero, One, Zero)) === None)
          assert(t.decode(Seq(Zero)) === None)
          assert(t.decode(Seq()) === Some(Seq()))
      }

      it should "meanLength good" in {
          val t = new Huffman[Char]("HOURRAHOURRAHOURRRRA").tree.get
          assert(t.meanLength === 2.2)

          // tree = 1feuille
          val t1 = new Huffman[Char]("a").tree.get
          assert(t1.meanLength === 1.0)

      }

      it should "exemple compress, uncompress HOURRAHOURRAHOURRRRA" in {
          val msg_original = "HOURRAHOURRAHOURRRRA"
          val h = new Huffman[Char](msg_original)
          val msg_compressed = h.compress(msg_original)
          val msg_uncompressed = h.uncompress(msg_compressed).get
          assert(h.tree.get.label === 20 )
          assert(math.abs(h.entropy - 2.17) < 0.001 )
          assert(msg_uncompressed === msg_original.toList)
      }

      it should "exemple compress, uncompress RESEAUX" in {
          val msg_original = "RUE"
          val h = new Huffman[Char]("RESEAUX")
          val msg_compressed = h.compress(msg_original)
          val msg_uncompressed = h.uncompress(msg_compressed).get
          assert(h.tree.get.label === 7 )
          assert(msg_uncompressed === msg_original.toList)
      }

      it should "exemple compress, uncompress MISSISSIPPI RIVER" in {
          val msg_original = "MISSISSIPPI RIVER"
          val h = new Huffman[Char](msg_original)
          val msg_compressed = h.compress(msg_original)
          val msg_uncompressed = h.uncompress(msg_compressed).get
          assert(h.tree.get.label === 17 )
          assert(msg_uncompressed === msg_original.toList)
      }

  }
