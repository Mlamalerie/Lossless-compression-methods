package compress.lz

object testLZ {
	type Dictionary = IndexedSeq[String]
	def uncompress(res : Seq[Int], dict0: Dictionary) = {
		def lecture(msg_received_total: Seq[Int], dict : Dictionary, decoded_result_total : Seq[String] = Seq(), cursor_msg : Int = 0, prof : Int = 0) : (Seq[String],Dictionary) = {
			print(s"PROF ${prof} **********************************")

			if(prof < 15 && cursor_msg < msg_received_total.length  ) {
				val x_int_received = msg_received_total(cursor_msg) // on s'interresse à quoi, In
				println(s"[${cursor_msg}]=${x_int_received} * dict(index=${x_int_received})=${dict(x_int_received)}")
				val d_res = dict(x_int_received)
				if(decoded_result_total.nonEmpty) {
					println("@ ")
					val c_ = d_res(0)
					val new_entry = decoded_result_total.last + c_
					// on ajoute au dictionnaire
					val new_dict = dict :+ new_entry
					println("new_entry",new_dict.indexOf(new_entry)+1,new_entry)
					lecture(msg_received_total, new_dict,decoded_result_total :+ d_res, cursor_msg+1, prof+1)
				} else {
					println("@@ ")
					lecture(msg_received_total, dict,decoded_result_total :+ d_res, cursor_msg+1, prof+1)
				}

			}else {
				println(" THE END")
				(decoded_result_total,dict)
			}
		}

		lecture(res,dict0)
	}

	def main(args: Array[String]): Unit = {

		//val msg = "ababbabcababba"
		//val msg = "TWEET TWEET"
		//val msg = "bbbabbaabbbb"
		//val init_dict = IndexedSeq("a", "b", "c")
		//val init_dict = IndexedSeq("E", "T", "W"," ")
		//val init_dict = IndexedSeq("a", "b", "d","n"," ")

		/*
		val init_dict = IndexedSeq("a", "b","c")
		val r  = new LZW(init_dict)
		val m = Seq(1,2,4,5,2,3,4,6,1)
		val res = r.uncompress(Seq(1,2,4,5,2,3,4,6,1).map(_ - 1))
		println(res)

		val msg = "bbbabbaabbbb"
		val l = new LZW(IndexedSeq("a", "b","c"))
		//println(l.compress(msg))
		println(l.uncompress(Seq(1, 2, 0, 3, 4, 2, 1)).mkString(""))*/
		val msg = ""
		val l = new LZW()
		println(l.uncompress(l.compress(msg)))
	}

}
