def sum(list: List[Int]): Int = list match {
	case Nil => 0
	case n :: rest => println(n, rest); n + sum(rest)
}
val nums = List(1, 2, 3, 4, 5)

def compress[T](msg: Seq[T]): Seq[(T, Int)] = {
	msg.toList.foldLeft(Seq.empty[(T, Int)])((acc, cursor_msg) => acc match {
		// si l'accumulateur est vide
		case Nil => (cursor_msg, 1) +: Nil // ajouter un tuple initial.. List((a,1))

		// tête de acc :: corps  (j'arrache la tete du corp de l'acc)
		case (letter, count) :: corps => //println(acc,"#",cursor_msg);
			// si le curseur est sur la mm lettre là, on incremente
			if (letter == cursor_msg) (letter, count + 1) :: corps // je change la tete du corps
			// si le curseur est une nouvelle lettre
			else (cursor_msg, 1) +: acc //e
	}).reverse
}

def uncompress[T](seq_msg: Seq[(T, Int)]): Option[Seq[T]] = {
	def duplicate(x: T, nb: Int): Seq[T] = {
		if (nb == 0)
			Seq.empty[T]
		else
			x +: duplicate(x, nb - 1)
	}
	val seq_msg_uncompressed = seq_msg.toList.foldLeft(Seq.empty[T])((acc, elem) => acc ++ duplicate(elem._1, elem._2))
	// c carré ça ? demandé à la prof
	if (seq_msg == compress(seq_msg_uncompressed)) {
		Some(seq_msg_uncompressed)
	} else {
		None
	}
}

var l = Seq()


val msg_original = "aabccdeefgijjjklmnopqrrstttuvvwwxyyyzz"


val msg_compressed = compress(msg_original)
val msg_uncompressed = uncompress(msg_compressed)

print(" # msg => %s [length=%d]".format(msg_original.toString, msg_original.length))
print(" # compressed() => %s".format(msg_compressed.toString))
print("                   [length=%d]".format(msg_compressed.map(e => e._1.toString.length + e._2.toString.length).sum))
//msg_compressed.map(e => e._1.toString() + e._2).mkString("")
var gain = (msg_original.length - msg_compressed.map(e => e._1.toString.length + e._2.toString.length).sum).toFloat * 100 / msg_original.length

print(" # uncompressed() => %s".format(msg_uncompressed.toString))

// https://exercism.org/tracks/scala/exercises/run-length-encoding/solutions/86051bccf3504cb9960f64eaae63fc9e
