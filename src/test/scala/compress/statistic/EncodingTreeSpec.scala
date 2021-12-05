package compress.statistic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class EncodingTreeSpec extends AnyFlatSpec with Matchers
  {
    behavior of "compress.compress.statistic.EncodingTree"
      it must "exist along with its child case classes" in
        { "val t : EncodingTree[Char] = EncodingNode(2, EncodingLeaf(1, 'a'), EncodingLeaf(1, 'b'))" must compile }
      it must "contain methods `has` and `meanLength` with correct signature" in
        { """val t : EncodingTree[Char] = EncodingNode(2, EncodingLeaf(1, 'a'), EncodingLeaf(1, 'b'))
            |val hasResult : Boolean = t has 'a'
            |val len       : Double  = t.meanLength """.stripMargin must compile }
      it must "contains extended methods `encode`, `decodeOnce` and `decode` with correct signature" in
        { """val et : EncodingTree[Char] = EncodingNode(2, EncodingLeaf(1, 'a'), EncodingLeaf(1, 'b'))
            |val z : Option[Seq[Bit]]          = et encode 'a'
            |val o : Option[(Char, Seq[Bit])]  = et decodeOnce Seq.empty[Bit]
            |val s : Option[Seq[Char]]         = et decode     Seq.empty[Bit]""".stripMargin must compile }


      it should "has() good" in {
          val t = EncodingNode[Char](7, EncodingNode[Char](4, EncodingLeaf[Char](2,'L'), EncodingLeaf[Char](2,'M')), EncodingNode[Char](3, EncodingLeaf[Char](2,'A'), EncodingLeaf[Char](1,'I')))

          assert(t.has('L') === true)
          assert(t.has('M') === true)
          assert(t.has('m') === false)
          assert(t.has('x') === false)
        }

      it should "val dict_correspondence good" in {
        val t = EncodingNode[Char](7, EncodingNode[Char](4, EncodingLeaf[Char](2,'L'), EncodingLeaf[Char](2,'M')), EncodingNode[Char](3, EncodingLeaf[Char](2,'A'), EncodingLeaf[Char](1,'I')))

        assert(t.dict_correspondence('L') === Seq(Zero,Zero))
        assert(t.dict_correspondence('M') === Seq(Zero,One))
      }

      it should "pass encode() good" in {
          val t = EncodingNode[Char](7, EncodingNode[Char](4, EncodingLeaf[Char](2,'L'), EncodingLeaf[Char](2,'M')), EncodingNode[Char](3, EncodingLeaf[Char](2,'A'), EncodingLeaf[Char](1,'I')))
          assert(t.encode('L') === Some(Seq(Zero,Zero)))
          assert(t.encode('M') === Some(Seq(Zero,One)))
          assert(t.encode('x') === None)
      }


  }


