package compress.statistic
import compress.statistic.{Bit, EncodingLeaf, EncodingNode, EncodingTree, Huffman, One, Zero}
object test {



	def main(args: Array[String]): Unit = {


		var h = new Huffman[Char]("MISSISSIPI RIVER")
		//var h = new Huffman[Char]("HOURRAHOURRAHOURRRRA")
		//var h = new Huffman[Char]("DIDONDINADITONDUDOSDUNDODUDINDON")
		val msg_original = ""
		//var h = new Huffman[Char](msg_original)
		//println("...",h.orderedCounts.reverse.map( leaf => EncodingLeaf(leaf._2,leaf._1) ).toList)
		println(h.compress("RUE"))
		println(h.orderedCounts)
		println(h.tree.get.dict_correspondence)
		//println(h.occurrences)
/*
		println(h.entropy)


		val msg_compressed = h.compress(msg_original)
		println(msg_compressed)
		val msg_uncompressed = h.uncompress(msg_compressed)
		println(msg_uncompressed)
		//println(tree_mlamali.meanLength)
*/

		//println(tree.reduce((g,d) => d.toString + g.toString))
	}
}
