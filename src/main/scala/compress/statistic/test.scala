package compress.statistic
import compress.statistic.{Bit, EncodingLeaf, EncodingNode, EncodingTree, Huffman, One, Zero}
object test {



	def main(args: Array[String]): Unit = {

		def get_two_lowest_to(nodes_list: Seq[EncodingTree[Char]]) = {
			nodes_list.sortWith(_.reduceLabel((x,y,z) => x+y+z) > _.reduceLabel((x,y,z) => x+y+z)).takeRight(2).toList
		}
		def create_one_node(g : EncodingTree[Char],d: EncodingTree[Char])  = {
			val value = g.label + d.label
			EncodingNode(value,g,d)
		}
		def delete_one_node_to(drop : EncodingTree[Char],nodes_list : List[EncodingTree[Char]])  = {
			nodes_list.filter( _ != drop)
		}
		def delete_nodes_to(drops : List[EncodingTree[Char]],nodes_list : List[EncodingTree[Char]])  = {
			nodes_list.filter( !drops.contains(_) )
		}
		def add_one_node_to(adding: EncodingTree[Char],nodes_list : List[EncodingTree[Char]])  = {
			 adding +: nodes_list.toList
		}

		def construct_tree(nodes_list :  List[EncodingTree[Char]]) : EncodingTree[Char] = {
				if (nodes_list.length == 1) {
					nodes_list(0)
				}else {
					// les deux plus petit noeud
					val two_nodes = get_two_lowest_to(nodes_list )
					//println("imbeciles :",two_nodes)
					// supprimer les noeud de la list
					val nodes_list_without_two_nodes =  delete_nodes_to(two_nodes,nodes_list)
					// ajouter le nouveau noeud
					//println("nodes_list_without_two_nodes :",nodes_list_without_two_nodes)
					val new_node = create_one_node(two_nodes(0),two_nodes(1))
					// on recommence avec le nouveau liste
					//println("new_node :",new_node)
					// new list
					val new_res_list = add_one_node_to(new_node,nodes_list_without_two_nodes)
					//println("new_res_list :",new_res_list.length)
					construct_tree(new_res_list)
				}
		}


		//val leafOrdered = h.orderedCounts.reverse.map( leaf => EncodingLeaf(leaf._2,leaf._1) ).toList
		//val two_low = get_two_lowest_to(leafOrdered )
		//println("leafOrdered ",leafOrdered ,leafOrdered.length)
		//println("*",create_one_node(two_low(0),two_low(1)))
		//println("delete      ", delete_nodes_to(two_low,leafOrdered)  )
		//println("add         ", add_one_node_to(two_low(0), leafOrdered) )
		//var h = new Huffman[Char]("MLAMALI")
		var h = new Huffman[Char]("HOURRAHOURRAHOURRRRA")
		println("...",h.orderedCounts.reverse.map( leaf => EncodingLeaf(leaf._2,leaf._1) ).toList)
		val h_tree = h.tree.get
		println("#final#",h_tree)
		println(h_tree.decodeOnce(List(Zero)))
		println(h_tree.reduceWith(x=> x.toString)((g,d) => g + d))
		println(h_tree.decodeOnce(Seq(One,Zero)))

		//println(tree.reduce((g,d) => d.toString + g.toString))
	}
}
