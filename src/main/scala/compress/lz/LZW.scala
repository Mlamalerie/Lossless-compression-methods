package compress.lz

import compress.{Compressor, CustomException}
import Dictionaries._

/** The LZW compression method
 *
 * @param initialDictionary the starting dictionary
 */
class LZW(val initialDictionary: Dictionary = ASCII) extends Compressor[Char, Seq[Int]] {
	/** @inheritdoc */
	def compress(msg: Seq[Char]): Seq[Int] = {
		def verify_msg(msg: Seq[Char]): Boolean = {
			println(msg.toList.forall(x => this.initialDictionary.contains(x.toString)))
			msg.toList.forall(x => this.initialDictionary.contains(x.toString))
		}

		def lecture(msg_total: Seq[Char], dict: Dictionary, encoded_result: Seq[Int] = Seq(), cursor_l: Int = 0, cursor_r: Int = 1, prof: Int = 0): Seq[Int] = {
			println(s"PROF ${prof} *********[${cursor_l}, ${cursor_r}]")

			val select_x_str = msg_total.slice(cursor_l, cursor_r + 1).toString() // on s'interresse à quoi
			println("select_x_str", select_x_str, dict.indexOf(select_x_str))
			//abbabc
			if (cursor_r < msg_total.length) {
				dict.indexOf(select_x_str) match {
					// si inconnu dans le dico,
					case i: Int if i == -1 => {
						// TODO supprimer les val

						//  alors nouvelle séquence ajouté au dico,
						val new_dict = dict :+ select_x_str
						// l'ouput s'obtiens en récuperant l'index de x[:-1]
						val select_x_str_ancetre = msg_total.slice(cursor_l, cursor_r).toString()
						val output = new_dict.indexOf(select_x_str_ancetre)

						println("new_dict histo", new_dict.slice(new_dict.length - 3, new_dict.length))
						println("select_x_str_ancetre", select_x_str_ancetre)
						print("output", output)
						val add = cursor_r - cursor_l
						println("add", add)
						lecture(msg_total, new_dict, encoded_result :+ output, cursor_l + add, cursor_r + 1, prof + 1)
					}
					// si la séquence existe déjà dans le dico, alors
					case _ => {
						println("tu connais déjà")
						lecture(msg_total, dict, encoded_result, cursor_l, cursor_r + 1, prof + 1) // juste decale le  curseur
					}
				}
			} else {
				println("c'est la fin")
				encoded_result :+ dict.indexOf(select_x_str)
			}
		}
		// si le msg contient des caractère louche
		verify_msg(msg) match {
			case true => if(msg.isEmpty) Seq.empty[Int] else lecture(msg, this.initialDictionary) // on a fait le choix de renvoyer un séquence vide si le message est vide
			case _ => throw CustomException("Error : msg saisie n'est pas valable. Votre msg contient surement des symboles inconnus au dictionnaire initial")
		}

	}

	/** @inheritdoc */
	def uncompress(res: Seq[Int]): Option[Seq[Char]] = {

		def lecture(msg_received_total: Seq[Int], dict: Dictionary, decoded_result_total: Seq[String] = Seq(), cursor_msg: Int = 0, prof: Int = 0): Seq[Char] = {
			print(s"PROF ${prof} **********************************[${cursor_msg}]=")
			if (prof < 30 && cursor_msg < msg_received_total.length) {
				val x_int_received = msg_received_total(cursor_msg) // on s'interresse à quoi, In
				// si on est pas au début du processus
				if (decoded_result_total.nonEmpty) {
					val d_res = if(x_int_received < dict.length ) dict(x_int_received) else decoded_result_total.last + decoded_result_total.last(0)
					val c_ = d_res(0) // bail à ajouter à la fin de la nouvelle entree
					val new_entry = decoded_result_total.last + c_ //
					// on ajoute au dictionnaire
					val new_dict = dict :+ new_entry
					println("new_entry", new_dict.indexOf(new_entry), new_entry)

					lecture(msg_received_total, new_dict, decoded_result_total :+ d_res, cursor_msg + 1, prof + 1)
				}
				// si est au début du processus
				else {
					println("\n@ début du processus !")
					lecture(msg_received_total, dict, decoded_result_total :+ dict(x_int_received), cursor_msg + 1, prof + 1)
				}
			} else {
				println(" THE END")
				decoded_result_total.mkString("").toList
			}
		}

		if(res.isEmpty) {
			None

		} else {
			Some(lecture(res, this.initialDictionary))
		}
	}
}


