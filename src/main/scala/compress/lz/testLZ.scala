package compress.lz

object testLZ {
	type Dictionary = IndexedSeq[String]


	def main(args: Array[String]): Unit = {
		//val msg = "ababbabcababba"
		//val msg = "TWEET TWEET"
		val msg = "bbbabbaabbbb"
		//val init_dict = IndexedSeq("a", "b", "c")
		//val init_dict = IndexedSeq("E", "T", "W"," ")
		//val init_dict = IndexedSeq("a", "b", "d","n"," ")
		val init_dict = IndexedSeq("a", "b")
		val ascii = (0 to 255) map (_.toChar.toString)
		val r  = new LZW(init_dict)
		println(r.compress(msg))


		//println(r.compress("BABAABAAA"))


	}

}
