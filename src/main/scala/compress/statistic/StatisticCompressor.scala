package compress.statistic

import compress.{Compressor, CustomException}

/** A compress.statistic compressor relies on the statistics of symbols in source
  * @param source the input source
  * @tparam S type of symbol used in source
  */
abstract class StatisticCompressor[S](source : Seq[S]) extends Compressor[S, Seq[Bit]]
  {
    /** A map giving the occurrences of each symbol in the source sequence */
    val occurrences : Map[S, Int] = {
      source.groupBy(l => l).map(t => (t._1, t._2.length))
    }

    /** SHANNON entropy of source */
    val entropy : Double = {
      def log2(x: Double): Double = scala.math.log(x) / scala.math.log(2)
      -occurrences.map( c => {
        //println("--",c)
        c._2.toDouble / source.length * log2(c._2.toDouble / source.length)
      }).sum
    }

    /** The sequence of occurrences sorted by count */
    val orderedCounts : Seq[(S, Int)] = {
      occurrences.toList.sortWith((a,b) => a._2 > b._2) // trier par count uniquement
     }

    /** The encoding tree (in most cases, depends from `source`) */
    def tree : Option[EncodingTree[S]]

    /** @inheritdoc */
    def compress(msg: Seq[S]): Seq[Bit] = this.tree match {
          // si arbre est bien défini
           case Some(the_tree: EncodingTree[S]) =>
             // pour chaque symbol du message
             msg.map(c => {
               the_tree.encode(c) match {
                 case Some(ch_encoded: Seq[Bit]) => ch_encoded
                 case _ => throw CustomException("le symbole " + c.toString + " n'est pas encodable par notre arbre. En effet, veuillez saisir un mot qui contient des symboles connus")
               }
             }).toList.flatten

           // si arbre est pas bien défini
           case _ => Seq()
    }


    /** @inheritdoc */
    def uncompress(res: Seq[Bit]): Option[Seq[S]] = this.tree match {
      // si arbre est bien défini
      case Some(the_tree: EncodingTree[S]) =>
        // pour chaque symbol du message encodé
        the_tree.decode(res) match {
          case Some(msg_decoded: Seq[S]) => Some(msg_decoded)
          case _ => None
        }

      // si arbre est pas bien défini
      case _ => None
    }

  }
