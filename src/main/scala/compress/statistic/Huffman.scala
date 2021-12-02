package compress.statistic

import scala.collection.View.Empty

/** The HUFFMAN compression method */
class Huffman[S](source : Seq[S]) extends StatisticCompressor[S](source)
  {


    /** @inheritdoc */
    lazy val tree : Option[EncodingTree[S]] =
    {
      val get_two_lowest_to = (nodes_list: Seq[EncodingTree[S]]) => {
        nodes_list.sortWith(_.label > _.label).takeRight(2).toList
      }
      val create_one_node = (g : EncodingTree[S],d: EncodingTree[S])  => {
        val value = g.label + d.label
        EncodingNode(value,g,d)
      }
      val delete_nodes_to = (drops : List[EncodingTree[S]],nodes_list : List[EncodingTree[S]])  => {
        nodes_list.filter( !drops.contains(_) )
      }
      val add_one_node_to = (adding: EncodingTree[S],nodes_list : List[EncodingTree[S]])  => {
        adding +: nodes_list
      }
      def construct_tree(nodes_list :  List[EncodingTree[S]]) : EncodingTree[S] = {
        //println("#########################")
        if (nodes_list.length == 1) {
          nodes_list(0)
        }else {
          // les deux plus petit noeud
          val two_nodes = get_two_lowest_to(nodes_list )
          //println("imbeciles :",two_nodes)
          // supprimer les noeud de la list
          val nodes_list_without_two_nodes =  delete_nodes_to(two_nodes,nodes_list)
          // ajouter le nouveau noeud
          //println("nodes_list_without_two_nodes :",nodes_list_without_two_nodes)
          val new_node = create_one_node(two_nodes(0),two_nodes(1))
          // on recommence avec le nouveau liste
          //println("new_node :",new_node)
          // new list
          val new_res_list = add_one_node_to(new_node,nodes_list_without_two_nodes)
          //println("new_res_list :",new_res_list.length)
          construct_tree(new_res_list)
        }
      }

      val leafsOrdered = this.orderedCounts.reverse.map( leaf => EncodingLeaf(leaf._2,leaf._1) ).toList
      leafsOrdered match {
        case leafs : List[EncodingTree[S]] if(leafs.length) > 0 => {
          leafs match {
            case good_leafs if (leafs.size > 0) => Some(construct_tree(leafsOrdered))
            case _ => None
          }
        }
        case _ => None
      }

    }
  }


