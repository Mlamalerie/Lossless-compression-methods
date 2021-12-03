package compress.statistic

import scala.annotation.tailrec

/** The SHANNON-FANO compression method */
class ShannonFano[S](source : Seq[S]) extends StatisticCompressor[S](source)
  {
    /** @inheritdoc */
    lazy val tree : Option[EncodingTree[S]] = {
      @tailrec
      def two_equitable_part(tab : List[EncodingTree[S]],  i_g : Int =0, i_d : Int=0, g :  List[EncodingTree[S]] = Nil , d : List[EncodingTree[S]] = Nil ): (List[EncodingTree[S]],List[EncodingTree[S]]) = {
        val tab_sorted = tab.sortWith( (a,b) => a.label < b.label)

        val sum_g = g.map(_.label).sum
        val sum_d = d.map(_.label).sum
        //println(i_g, i_d, " - ", g,  sum_g , " ; ", d,  sum_d)
        if( (g.length + d.length) == tab_sorted.length) {
          (g,d)
        } else {
          if(sum_g <= sum_d) {
            two_equitable_part(tab_sorted,i_g+1,i_d,g :+ tab(i_g),d)
          } else {
            two_equitable_part(tab_sorted,i_g,i_d+1,g,tab.reverse(i_d) +: d)
          }
        }
      }
      def construct_tree_shanoon(tab : List[EncodingTree[S]]) : EncodingTree[S] = {
        if(tab.length >= 2) {
          val (part_1,part_2) = two_equitable_part(tab)
          val new_label = part_1.map(x => x.label).sum +  part_2.map(x => x.label).sum
          EncodingNode(new_label,construct_tree_shanoon(part_1),construct_tree_shanoon(part_2))
        } else {
          tab.head
        }
      }
      val leafsOrdered = this.orderedCounts.reverse.map( leaf => EncodingLeaf(leaf._2,leaf._1) ).toList
      leafsOrdered match {
        // si la list de feuille est bien défini avec au moins une feuille dedans
        case leafs : List[EncodingTree[S]] if leafs.nonEmpty => construct_tree_shanoon(leafs) match {
          case result_tree : EncodingTree[S] => Some(result_tree)
          case _ => None
        }
        case _ => None
      }
    }
  }
