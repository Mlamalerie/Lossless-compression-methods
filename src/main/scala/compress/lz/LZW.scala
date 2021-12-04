package compress.lz

import compress.{Compressor, CustomException}
import Dictionaries._

/** The LZW compression method
  * @param initialDictionary the starting dictionary
  */
class LZW(val initialDictionary : Dictionary = ASCII) extends Compressor[Char, Seq[Int]]
  {
    /** @inheritdoc */
    def compress(msg : Seq[Char]) : Seq[Int] = {
      def verify_msg(msg : Seq[Char]) : Boolean = {
        println(msg.toList.forall(x => this.initialDictionary.contains(x.toString)))
        msg.toList.forall(x => this.initialDictionary.contains(x.toString))
      }

      def lecture( msg_total: Seq[Char], dict : Dictionary,encoded_result : Seq[Int] = Seq(), cursor_l : Int = 0, cursor_r : Int = 1,prof : Int = 0) : Seq[Int] = {
        println(s"PROF ${prof} *********[${cursor_l}, ${cursor_r}]")

        val select_x_str = msg_total.slice(cursor_l, cursor_r + 1).toString() // on s'interresse à quoi
        println("select_x_str",select_x_str,dict.indexOf(select_x_str))

        if(cursor_r < msg_total.length) {
          dict.indexOf(select_x_str) match {
            // si inconnu dans le dico,
            case i: Int if i == -1 => {
              // TODO supprimer les val

              //  alors nouvelle séquence ajouté au dico,
              val new_dict = dict :+ select_x_str
              // l'ouput s'obtiens en récuperant l'index de x[:-1]
              val select_x_str_ancetre = msg_total.slice(cursor_l, cursor_r).toString()
              val output = new_dict.indexOf(select_x_str_ancetre)

              println("new_dict histo", new_dict.slice(new_dict.length -3,new_dict.length))
              println("select_x_str_ancetre", select_x_str_ancetre)
              print("output", output)
              val add = cursor_r - cursor_l
              println("add",add)
              lecture(msg_total, new_dict,encoded_result :+ output, cursor_l+add, cursor_r+1, prof+1)
            }
            // si la séquence existe déjà dans le dico, alors
            case _ => {
              println("tu connais déjà")
              lecture(msg_total, dict,encoded_result, cursor_l, cursor_r+1, prof+1)
            }
          }
        }else {
          println("c'est la fin")
          encoded_result :+ dict.indexOf(select_x_str)
        }
      }
      // si le msg contient des caractère louche
      verify_msg(msg) match {
        case true => lecture(msg,this.initialDictionary)
        case _ => throw CustomException("Error : msg saisie n'est pas valable. Votre msg contient surement des symboles inconnu aux dictionnaire initial")
      }

    }

    /** @inheritdoc */
    def uncompress(res : Seq[Int]) : Option[Seq[Char]] = None
  }


