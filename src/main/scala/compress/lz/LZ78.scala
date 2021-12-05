package compress.lz

import compress.Compressor
import Dictionaries._

import scala.annotation.tailrec
/** The LZ78 compression method */
object LZ78 extends Compressor[Char, Seq[(Int, Char)]]
  {
    /** @inheritdoc */
    def compress(msg : Seq[Char]) : Seq[(Int, Char)] = {
      @tailrec
      def lecture(msg_total: Seq[Char], dict: Dictionary, encoded_result: Seq[(Int, Char)] = Seq.empty[(Int, Char)], cursor_l: Int = 0, cursor_r: Int = 0): Seq[(Int, Char)] = {
        //println(s"\nPROF ${prof} *********[${cursor_l}, ${cursor_r}]")
        val select_x_str = msg_total.slice(cursor_l, cursor_r + 1).toString() // on s'interresse à quoi
        //println("select_x_str", select_x_str, dict.indexOf(select_x_str))
        if (cursor_r < msg_total.length) {
          dict.indexOf(select_x_str) match {
            // si inconnu dans le dico,
            case i: Int if i == -1 => {
              //  alors nouvelle séquence ajouté au dico,
              val new_dict = dict :+ select_x_str

              val select_x_str_ancetre = msg_total.slice(cursor_l, cursor_r).toString()
              val input_int = select_x_str_ancetre match {
                case anc : String if!anc.isEmpty => new_dict.indexOf(anc)
                case _ => 0
              }
              //println("select_x_str_ancetre",select_x_str_ancetre)
              //println("msg_total(cursor_r)",msg_total(cursor_r))
              val output = (input_int, select_x_str.last)
              //println("output",output)
              // l'élement sur le cursor_r
              val add = new_dict.indexOf(msg_total(cursor_r)) match {
                // si il est pas dans le dict
                case i: Int if i == -1 => 1
                // si il est dans le dict
                case _ => select_x_str.length
              }
              //println("add", add)
              lecture(msg_total, new_dict, encoded_result :+ output, cursor_l + select_x_str.length, cursor_r + add)
            }
            // si la séquence existe déjà dans le dico, alors
            case _ => {
              //println("@ tu connais déjà")
              lecture(msg_total, dict, encoded_result , cursor_l, cursor_r + 1) // juste decale le  curseur
            }
          }
        } else {
          //println("c'est la fin")
          if(select_x_str.isEmpty) {
            encoded_result
          } else{
            val input_int = select_x_str.slice(0,select_x_str.length-1) match {
              case anc : String if anc.nonEmpty => dict.indexOf(anc)
              case _ => 0
            }
            encoded_result :+ (input_int,select_x_str.last)
          }
        }
      }
      // si le msg contient des caractère louche
      if(msg.isEmpty) {
        Seq.empty[(Int,Char)]
      } else {
        lecture(msg, Dictionaries.empty)
      }
    }
    /** @inheritdoc */
    def uncompress(res : Seq[(Int, Char)]) : Option[Seq[Char]] = {
      def lecture(msg_received_total: Seq[(Int, Char)], dict: Dictionary, decoded_result_total: Seq[String] = Seq(), cursor_msg: Int = 0): Seq[Char] = {
        //println(s"PROF ${prof} **********************************[${cursor_msg}]=")
        if (cursor_msg < msg_received_total.length) {
          val x_encoded_tuple = msg_received_total(cursor_msg) // on s'interresse à quoi, (Int,Char)
          //println("x_encoded_tuple", x_encoded_tuple)
          // si l'index existe dans le dictionnaire
          if(x_encoded_tuple._1 < dict.length) {
            //println("@")
            val g_ = dict(x_encoded_tuple._1)
            val d_ = x_encoded_tuple._2 // bail à ajouter à la fin de la nouvelle entree
            val new_entry = g_ + d_ //
            val new_dict = dict :+ new_entry // on ajoute au dictionnaire
            //println("new_entry", new_dict.indexOf(new_entry), new_entry,dict(x_encoded_tuple._1))
            lecture(msg_received_total, new_dict, decoded_result_total :+ new_entry, cursor_msg + 1)
          }
          // si l'index existe pas dans le dico
          else {
            //println("@@")
            val new_dict = dict :+ x_encoded_tuple._2.toString // on ajoute au dictionnaire
            lecture(msg_received_total, new_dict, decoded_result_total :+ x_encoded_tuple._2.toString, cursor_msg + 1)
          }

        } else {
          //println(" * THE END *")
          //println(decoded_result_total.length,dict.length)
          decoded_result_total.mkString("").toList
        }
      }

      if(res.isEmpty) {
        None
      } else {
        Some(lecture(res, Dictionaries.empty))
      }
    }
  }
