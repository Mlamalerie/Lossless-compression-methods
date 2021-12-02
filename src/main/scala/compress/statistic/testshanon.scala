package compress.statistic

object testshanon {
	def main(args: Array[String]): Unit = {
		def two_equitable_part(tab : List[EncodingTree[Char]],  i_g : Int =0, i_d : Int=0, g :  List[EncodingTree[Char]] = Nil , d : List[EncodingTree[Char]] = Nil ): (List[EncodingTree[Char]],List[EncodingTree[Char]]) = {
			val tab_sorted = tab.sortWith( (a,b) => a.label < b.label)

			val sum_g = g.map(_.label).sum
			val sum_d = d.map(_.label).sum
			//println(i_g, i_d, " - ", g,  sum_g , " ; ", d,  sum_d)
			if( (g.length + d.length) == tab_sorted.length) {
				(g,d)
			} else {
				if(sum_g <= sum_d) {
					two_equitable_part(tab_sorted,i_g+1,i_d,g :+ tab(i_g),d)
				} else {
					two_equitable_part(tab_sorted,i_g,i_d+1,g,tab.reverse(i_d) +: d)
				}
			}
		}
		def loop(tab : List[EncodingTree[Char]], tree : EncodingTree[Char] = EncodingLeaf(0,'c')) : Any = {
			if(tab.length >= 2) {
				val two_part = two_equitable_part(tab)
				List(loop(two_part._1,tree)) :: List(loop(two_part._2,tree))
			} else {
				tab
			}
		}

		def construct_tree_shanoon(tab : List[EncodingTree[Char]]) : EncodingTree[Char] = {
			if(tab.length >= 2) {
				val (part_1,part_2) = two_equitable_part(tab)
				val new_label = part_1.map(x => x.label).sum +  part_2.map(x => x.label).sum
				EncodingNode(new_label,construct_tree_shanoon(part_1),construct_tree_shanoon(part_2))
			} else {
				tab(0)
			}
		}
		val msg_original = "DIDONDINADITONDUDOSDUNDODUDINDON"
		//val msg_original = "DI"
		var shf = new ShannonFano[Char](msg_original)
		//val shf_tree = shf.tree.get
		val leafOrdered = shf.orderedCounts.reverse.map( leaf => EncodingLeaf(leaf._2,leaf._1) ).toList
		//val two_low = get_two_lowest_to(leafOrdered )
		println("leafOrdered ",leafOrdered ,leafOrdered.length)
		val x = two_equitable_part(leafOrdered)
		println(x)
		println("##############################################")
		val caca = construct_tree_shanoon(leafOrdered)
		println("#->",caca)
		//val m = caca(0).toLis
		//println("# ---->",)


		//println("#final#",shf_tree)
		//var sequence_bits = List(Zero, One,Zero)
		//println(shf_tree.decodeOnce(sequence_bits))
	}
}
