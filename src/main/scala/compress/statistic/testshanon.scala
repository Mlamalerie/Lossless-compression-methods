package compress.statistic

import compress.CustomException

object testshanon {
	def main(args: Array[String]): Unit = {

		//val msg_original = "Mlamali"
		val msg_original = "HOURRAHOURRAHOURRRRA"
		println(msg_original.length)
		println("###############################################################")
		var h = new Huffman[Char](msg_original ) //println("...",h.orderedCounts.reverse.map( leaf => EncodingLeaf(leaf._2,leaf._1) ).toList)
		println(h.orderedCounts)
		println(h.tree)
		println(h.compress(msg_original).map(x => {if (x == Zero) 0 else 1}))
		println(h.compress(msg_original).length)
		println(h.tree.get.dict_correspondence)
		println(h.tree.get.meanLength)
		println(h.entropy)
		println("###############################################################\n")
		var h1 = new ShannonFano[Char](msg_original ) //println("...",h.orderedCounts.reverse.map( leaf => EncodingLeaf(leaf._2,leaf._1) ).toList)
		println(h1.orderedCounts)
		println(h1.tree)
		println(h1.compress(msg_original).map(x => {if (x == Zero) 0 else 1}))
		println(h1.compress(msg_original).length)
		println(h1.tree.get.dict_correspondence)
		println(h1.tree.get.meanLength)
		println(h1.entropy)


	}
}
