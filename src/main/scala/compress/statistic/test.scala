package compress.statistic
import compress.statistic.{Bit, EncodingLeaf, EncodingNode, EncodingTree, Huffman, One, Zero}
object test {



	def main(args: Array[String]): Unit = {


		//var h = new Huffman[Char]("MISSISSIPI RIVER")
		//var h = new Huffman[Char]("HOURRAHOURRAHOURRRRA")
		val msg_original = "a"
		var h = new Huffman[Char](msg_original)

		//println("...",h.orderedCounts.reverse.map( leaf => EncodingLeaf(leaf._2,leaf._1) ).toList)
		println(h.orderedCounts)
		println(h.tree)
		println(h.compress(msg_original))


		/*EncodingNode([32],
			EncodingNode([20], (D,11), EncodingNode([9], (O,5), (I,4))),
			EncodingNode([12], EncodingNode([6], EncodingNode([3], EncodingNode([2], (A,1), (T,1)), (S,1)), (U,3)), (N,6)))
*/
	}
}
