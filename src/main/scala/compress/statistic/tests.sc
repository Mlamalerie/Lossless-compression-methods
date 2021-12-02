import compress.Compressor
import compress.statistic.{Bit, EncodingLeaf, EncodingNode, EncodingTree, Huffman, One, Zero}

import scala.math

val msg_original = "MLAMALI"
var h = new Huffman[Char](msg_original)
//println("...",h.orderedCounts.reverse.map( leaf => EncodingLeaf(leaf._2,leaf._1) ).toList)
val tree_mlamali = h.tree.get
println("#final#",tree_mlamali)

println(h.entropy)
msg_original.map(c => {
	tree_mlamali.encode(c) match {
		case Some(char_encoded: Seq[Bit]) => char_encoded
		case _ => Nil
	}
}).flatten
val msg_compressed = h.compress(msg_original)