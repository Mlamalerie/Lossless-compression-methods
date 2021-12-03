package compress.statistic

import compress.CustomException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ShannonFanoSpec extends AnyFlatSpec with Matchers
  {
    behavior of "compressor.compress.statistic.ShannonFano"
     it must "exist" in
       { "val compressor : ShannonFano[Char] = new ShannonFano(Seq.empty[Char])" must compile }
     it must "contain methods `compress` and `uncompress` with correct signature" in
       { """val compressor : ShannonFano[Char] = new ShannonFano(Seq.empty[Char])
           |val z : Seq[Bit]          = compressor.compress(Seq.empty[Char])
           |val s : Option[Seq[Char]] = compressor.uncompress(z)""".stripMargin must compile }
     it must "contain fields `entropy`, `occurrences` and `orderedCounts` with correct type" in
       { """val compressor : ShannonFano[Char] = new ShannonFano(Seq.empty[Char])
           |val h : Double = compressor.entropy
           |val occs : Map[Char, Int] = compressor.occurrences
           |val o_occs : Seq[(Char, Int)] = compressor.orderedCounts
           |val t : Option[EncodingTree[Char]] = compressor.tree""".stripMargin must compile }


      it should "initiliazation good" in {
          val t = new ShannonFano[Char]("MLAMALI").tree
          assert(t !== None)
          val t2 = new ShannonFano[Char]("M").tree
          assert(t2 !== None)
          val t3 = new ShannonFano[Char]("").tree
          assert(t3 === None)

      }
      it should "val dict_correspondence good" in {
          val t = new ShannonFano[Char]("DIDONDINADITONDUDOSDUNDODUDINDON").tree.get

          assert(t.dict_correspondence('D').length == 2)
          assert(t.dict_correspondence('N').length == 2)
          assert(t.dict_correspondence('O').length == 3)


          // test arbre 2 nodes
          val t1 = new ShannonFano[Char]("AB").tree.get
          assert(t1.dict_correspondence('A').length === 1)
          assert(t1.dict_correspondence('B').length === 1)
          assert(t1.dict_correspondence('B') !== t1.dict_correspondence('A'))
          // test arbre 1 nodes
          val t2 = new ShannonFano[Char]("A").tree.get
          assert(t2.dict_correspondence('A').length === 1)


      }

      it should "val dict_correspondence must have unique values" in {
          val t = new ShannonFano[Char]("MLAMALI ARI MATHIEEUUU JOO REDOUAANE").tree.get
          val values = t.dict_correspondence.values.toList
          assert((values.map(x => values.count(_ != x)).sum == (values.length - 1)*values.length) === true)


          // test arbre 2 nodes
          val t1 = new ShannonFano[Char]("AB").tree.get
          val values1 = t.dict_correspondence.values.toList
          assert((values1.map(x => values.count(_ != x)).sum == (values1.length - 1)*values1.length) === true)


      }
      it should "encode() good" in {
          val t = new ShannonFano[Char]("DIDONDINADITONDUDOSDUNDODUDINDON").tree.get

          assert(t.encode('D') === Some(Seq(One,One)))
          assert(t.encode('N') === Some(Seq(One,Zero)))
          assert(t.encode('d') === None)
          assert(t.encode('x') === None)
      }

      it should "decodeOnce() good" in {
          val t = new  ShannonFano[Char]("MLAMALI").tree.get
          assert(t.decodeOnce(t.dict_correspondence('M') ++ t.dict_correspondence('L') ++ t.dict_correspondence('A')) === Some(('M',t.dict_correspondence('L') ++ t.dict_correspondence('A'))))
          assert(t.decodeOnce(t.dict_correspondence('A') ++ Seq(Zero,Zero,One,Zero)) === Some(('A',Seq(Zero,Zero,One,Zero))))
          assert(t.decodeOnce(t.dict_correspondence('I') ++ Seq(One)) === Some(('I',Seq(One))))
          assert(t.decodeOnce(Seq(Zero)) === None)
          assert(t.decodeOnce(Seq()) === None)

      }

      it should "decode() good" in {
          val t = new ShannonFano[Char]("DIDONDINADITONDUDOSDUNDODUDINDON").tree.get
          assert(t.decode(t.dict_correspondence('I') ++ t.dict_correspondence('I') ) === Some("II".toList))
          assert(t.decode(t.dict_correspondence('D') ++ t.dict_correspondence('O') ++ t.dict_correspondence('U')  ) === Some("DOU".toList))
          assert(t.decode(t.dict_correspondence('A') ) === Some("A".toList))
          assert(t.decode(t.dict_correspondence('S') ++ Seq(Zero)) === None)
          assert(t.decode(Seq(Zero)) === None)
          assert(t.decode(Seq()) === Some(Seq()))
      }

      it should "meanLength good" in {
          val t = new ShannonFano[Char]("DIDONDINADITONDUDOSDUNDODUDINDON").tree.get
          assert(t.meanLength === 2.625)

          // tree = 1feuille
          val t1 = new ShannonFano[Char]("a").tree.get
          assert(t1.meanLength === 1.0)
      }

      it should "exemple compress, uncompress DIDONDINADITONDUDOSDUNDODUDINDON" in {
          val msg_original = "DIDONDINADITONDUDOSDUNDODUDINDON"
          val h = new ShannonFano[Char](msg_original)
          val msg_compressed = h.compress(msg_original)
          val msg_uncompressed = h.uncompress(msg_compressed).get
          assert(h.tree.get.label === 32 )
          assert(math.abs(h.entropy - 2.565) < 0.001 )
          assert(msg_uncompressed === msg_original.toList)
      }
      it should "exemple compress, uncompress DINDE" in {
          val msg_original = "DINDE"
          val h = new ShannonFano[Char]("DIDONDINADITONDUDOSDUNDODUDINDON")
          assert({try {
              h.compress(msg_original)
          }
          catch {
              case c: CustomException => true
              case _ => false
          }} === true)
      }

      it should "exemple compress, uncompress MISSISSIPPI RIVER" in {
          val msg_original = "MISSISSIPPI RIVER"
          val h = new ShannonFano[Char](msg_original)
          val msg_compressed = h.compress(msg_original)
          val msg_uncompressed = h.uncompress(msg_compressed).get
          assert(h.tree.get.label === 17 )
          assert(msg_uncompressed === msg_original.toList)
      }
  }
