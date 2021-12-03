package compress.statistic

object testshanon {
	def main(args: Array[String]): Unit = {

		//val msg_original = "Mlamali"
		val msg_original = "DIDONDINADITONDUDOSDUNDODUDINDON"
		//val msg_original = "A"
		val shf = new ShannonFano[Char](msg_original)

		println("##############################################")
		//var values = shf.tree.get.dict_correspondence.values.toList
		var values = List(Seq(1,0), Seq(1,0), Seq(1,1))

		println(values.map(x => {
			values.count(_ != x)

		}).sum == (values.length - 1)*values.length )

		print()
		//println(shf.uncompress(shf.compress(msg_original)))
		//println("#final#",shf_tree)
		//var sequence_bits = List(Zero, One,Zero)
		//println(shf_tree.decodeOnce(sequence_bits))
	}
}
