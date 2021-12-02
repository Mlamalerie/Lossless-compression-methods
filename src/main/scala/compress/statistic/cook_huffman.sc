import compress.statistic.{Bit, EncodingLeaf, EncodingNode, EncodingTree, Huffman, One, ShannonFano, Zero}
var tree_mlamali = EncodingNode[Char](7, EncodingNode[Char](4, EncodingLeaf[Char](2,'L'), EncodingLeaf[Char](2,'M')), EncodingNode[Char](3, EncodingLeaf[Char](2,'A'), EncodingLeaf[Char](1,'I')))
print(tree_mlamali)
val add = (a: Int, b: Int) => a + b
def add2(a: Int, b: Int): Int = {
	a + b
}

def two_equitable_part(tab : List[EncodingTree[Char]],  i_g : Int =0, i_d : Int=0, g :  List[EncodingTree[Char]] = Nil , d : List[EncodingTree[Char]] = Nil ): (List[EncodingTree[Char]],List[EncodingTree[Char]]) = {
	val tab_sorted = tab.sortWith(_.label < _.label)

	val sum_g = g.map(_.label).sum
	val sum_d = d.map(_.label).sum
	println(i_g, i_d, " - ", g,  sum_g , " ; ", d,  sum_d)
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
def loop(tab : List[EncodingTree[Char]]) = {
	if(tab.length >= 2) {
		val two_part = two_equitable_part(tab)
		List(loop(two_part._1)) :: List(loop(two_part._2))
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
val caca = loop(leafOrdered)

lazy val dict_correspondence = {
	def construct_dict[S](tree : EncodingTree[S]) : Map[S,Seq[Bit]] = {
		val map_res = Map[S,Seq[Bit]]()
		def visiter(node : EncodingTree[S], bits_list : Seq[Bit]) : Map[S,Seq[Bit]] = {
			node match {
				case EncodingLeaf(_, v) => map_res  + (v -> bits_list) // si c'est une feuille
				case EncodingNode(_, l, r) => visiter(l, bits_list :+ Zero) ++ visiter(r, bits_list :+ One) // si c'est un noeud
			}
		}
		visiter(tree,Seq())
	}
	construct_dict(tree_mlamali)
}

def encode(x : Char) : Option[Seq[Bit]] = {
	if (dict_correspondence.contains(x)) {
		dict_correspondence.get(x)
	} else {
		None
	}
}

def decodeOnce(res : Seq[Bit]) : Option[(Char, Seq[Bit])] = {
	val d_inverse = dict_correspondence.map(_.swap)
	if (d_inverse.contains(res)) {
		Some((d_inverse.get(res).get,res))
	} else {
		None
	}
}

def decodeOnce2(res : Seq[Bit]) : Option[(Char, Seq[Bit])] = {

	def parcours(current_node : EncodingTree[Char],chemin : Seq[Bit]) : Option[Char] = {
		//println(chemin.length)
		current_node match {
			case EncodingLeaf(_, v) => {
				// si je suis arrivé à une feuille
				// alors si je suis arrivé au bout du chemin
				if(chemin.length == 0) {
					Some(v)
				} else {

					None
				}
			}
			case EncodingNode(_, l, r) => {
				if (chemin.length > 0) {
					chemin(0) match {
						case Zero => parcours(l, chemin.slice(1, chemin.length))
						case One => parcours(r, chemin.slice(1, chemin.length))
					}
				}else {
					None
				}
			} // si c'est un noeud
		}
	}
	// le caractère trouvé ou pas ?
	val c = parcours(tree_mlamali,res)
	if (c.isDefined) { // si oui
		Some((c.get,res))
	} else { //  sinon
		None
	}

}
println(decodeOnce2(List(Zero, One)))
println(decodeOnce2(List(Zero, One,Zero)))
println(decodeOnce2(List(Zero, One,One,One)))
println(decodeOnce2(List(One)))
println(decodeOnce2(Seq()))
println(decodeOnce2(Seq(Zero,Zero,Zero,Zero,Zero,One,One,One)))
println(decodeOnce2(Seq(Zero,Zero,Zero,Zero,Zero,One,One,One,One,One)))
println(decodeOnce2(Seq(Zero)))
println(decodeOnce2(Seq(One,Zero)))

def decode(res : Seq[Bit]) : Option[Seq[Char]] = {

	// return (le char, nombre de saut)
	def tree_find_char(current_node: EncodingTree[Char], sequence: Seq[Bit], prof: Int = 0): Option[(Char, Int)] = {
		//println(sequence.length)
		current_node match {
			// si je suis arrivé à une feuille
			case EncodingLeaf(_, v) => Some((v, prof))

			case EncodingNode(_, l, r) => {
				if (sequence.length > 0) {
					sequence(0) match {
						case Zero => tree_find_char(l, sequence.slice(1, sequence.length), prof + 1)
						case One => tree_find_char(r, sequence.slice(1, sequence.length), prof + 1)
					}
				} else {
					None
				}

			} // si c'est un noeud
		}
	}

	def all_run(sequence: Seq[Bit],acc_seq_result : Seq[Char] = Seq.empty[Char]) : Option[Seq[Char]] = {
		// si il reste encore du chemin à faire

		if (sequence.length > 0) {
			//println("*")
			val char_and_cursor = tree_find_char(tree_mlamali, sequence)
			//println("***")
			// si la lettre est trouvé
			if (char_and_cursor.isDefined) { // si oui
				//println("**")
				val le_char = char_and_cursor.get._1
				val le_cursor = char_and_cursor.get._2


				all_run(sequence.slice(le_cursor,sequence.length),acc_seq_result :+ le_char )

			} // si oh chakal ya pas la lettre dans mon arbre la elle existe pas
			else {

				None
			}
		} else {

			if(acc_seq_result.isEmpty) {
				None
			} else {
				Some(acc_seq_result)
			}

		}

	}
	// le caractère trouvé ou pas ?
	all_run(res) // TODO faire des rise error selon si ya une lettre qui est pas reconnu
}

print(decode(List(Zero)))
print(decode(Seq()))
print(decode(Seq(Zero,Zero,Zero,Zero,Zero,One,One,One)))
print(decode(Seq(Zero,Zero,Zero,Zero,Zero,One,One,One,One,One)))
print(decode(Seq(Zero)))
print(decode(Seq(One,Zero)))
