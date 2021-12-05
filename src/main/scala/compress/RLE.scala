package compress

/** The Run-length encoding compression method */
class RLE[T] extends Compressor[T, Seq[(T, Int)]] {
	/** @inheritdoc */
	def compress(msg: Seq[T]): Seq[(T, Int)] = {
		msg.toList.foldLeft(List.empty[(T, Int)])((acc, cursor_msg) => acc match {
			// si l'accumulateur est vide
			case Nil => (cursor_msg, 1) :: Nil // ajouter un tuple initial.. List((a,1))

			// tête de acc :: corps  (j'arrache la tete du corp de l'acc)
			case (letter, count) :: corps => //println(acc, "#", cursor_msg);
				// si le curseur est sur la mm lettre là, on incremente
				if (letter == cursor_msg) (letter, count + 1) :: corps // je change la tete du corps
				// si le curseur est une nouvelle lettre
				else (cursor_msg, 1) :: acc //
		}
			// passe l'element suivant du message
		).reverse
	}

	/** @inheritdoc */
	def uncompress(seq_msg: Seq[(T, Int)]): Option[Seq[T]] = {
		def duplicate(x: T, nb: Int): Seq[T] = {
			if (nb == 0)
				Seq.empty[T]
			else
				x +: duplicate(x, nb - 1)
		}
		val seq_msg_uncompressed = seq_msg.toList.foldLeft(Seq.empty[T])((acc, elem) => acc ++ duplicate(elem._1, elem._2))
		// c carré ça ? demandé à la prof
		seq_msg_uncompressed match {
			case result: Seq[T] => Some(result)
			case _ => None
		}
	}
}


