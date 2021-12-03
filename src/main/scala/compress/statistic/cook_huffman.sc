import compress.statistic.{Bit, EncodingLeaf, EncodingNode, EncodingTree, Huffman, One, ShannonFano, Zero}
var tree_mlamali = EncodingNode[Char](7, EncodingNode[Char](4, EncodingLeaf[Char](2,'L'), EncodingLeaf[Char](2,'M')), EncodingNode[Char](3, EncodingLeaf[Char](2,'A'), EncodingLeaf[Char](1,'I')))
print(tree_mlamali)
val msg_original = "DIDONDINADITONDUDOSDUNDODUDINDON"
msg_original.length
var h = new ShannonFano[Char](msg_original)

//println("...",h.orderedCounts.reverse.map( leaf => EncodingLeaf(leaf._2,leaf._1) ).toList)
println(h.orderedCounts)
println(h.tree)
h.compress(msg_original).map(x => {if (x == Zero) 0 else 1})
h.compress(msg_original).length
h.orderedCounts
h.tree.get.meanLength