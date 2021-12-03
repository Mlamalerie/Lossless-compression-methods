package compress.statistic

import scala.annotation.tailrec

/** Trait for binary encoding trees (integer-labeled binary trees)
 *
 * @tparam S type of symbols (in leaves only)
  */
sealed abstract class EncodingTree[S](val label : Int)
  {
    /* OPERATIONS ON VALUES */

    /** Checks if tree contains given value
      * @param x value to search
      * @return true if the tree has a leaf with value `x`
      */
    def has(x : S) : Boolean = this match
    {
      case EncodingLeaf(_, v   ) => v == x // si c'est une feuille
      case EncodingNode(_, l, r) => l.has(x) || r.has(x) // si c'est un noeud
      case _ => false
    }
    // TODO vérifier comment le parcours se fait : préfixe ou infixe ? sur le rapport cava pt, et préfixe c'est mieux psk il va trouver les valeur plus rapidement

    /** Reduce operation on tree values when applying a function on leaves beforehand
      * @param f the function applied to each leaf value
      * @param op the aggregation operation on a node
      * @tparam U the result type of `f`
      * @return the aggregated value of the tree
      */
    def reduceWith[U](f : S => U)(op : (U, U) => U) : U = this match
      {
        case EncodingLeaf(_, v   ) => f(v) // si c'est une feuille // ici f() return juste la valeur de la feuille
        case EncodingNode(_, l, r) => op((l reduceWith f)(op), (r reduceWith f)(op)) // si c'est un noeud
      }

    /** Reduce operation on tree values
      *
      * `t reduce op` is a shorthand for `(t reduceWith {v => v})(op)`
      * @param op the aggregation operation on a node
      * @return the aggregated value of the tree
      */
    def reduce(op : (S, S) => S) : S = (this reduceWith {v => v})(op)

    /* OPERATIONS ON LABELS */
    /** Reduce operation on tree labels when applying a function on leaves beforehand
      * @param fL the function applied to each leaf label
      * @param opL the aggregation operation on a node : first parameter is the node label
      * @tparam A the result type of `f`
      * @return the result of aggregation operation recursively applied to tree
      */
    def reduceLabelWith[A](fL : Int => A)(opL : (Int, A, A) => A) : A = this match
      {
        case EncodingLeaf(lbl, _   ) => fL(lbl)
        case EncodingNode(lbl, l, r) => opL(lbl, (l reduceLabelWith fL)(opL), (r reduceLabelWith fL)(opL))
      }

    /** Reduce operation on tree labels
      *
      * `t reduceLabel opL` is a shorthand for `(t reduceLabelWith {lbl => lbl})(opL)`
      * @param opL the aggregation operation on a node : first parameter is the node label
      * @return the aggregated label of the tree
      */
    def reduceLabel(opL : (Int, Int, Int) => Int) : Int = (this reduceLabelWith identity)(opL)


    /* ENCONDING/DECODING OPERATIONS */

    /* dictionnaire d'encodage */
    lazy val dict_correspondence : Map[S,Seq[Bit]] = {
      def construct_dict(root_tree : EncodingTree[S]) : Map[S,Seq[Bit]] = {
        // fonction qui va parcourir l'arbre à la recherche de feuille
        def visiter(node : EncodingTree[S], bits_list : Seq[Bit]) : Map[S,Seq[Bit]] = node match {
            case EncodingLeaf(_, v) => Map[S,Seq[Bit]]()  + (v -> bits_list) // si c'est une feuille
            case EncodingNode(_, l, r) => visiter(l, bits_list :+ Zero) ++ visiter(r, bits_list :+ One) // si c'est un noeud
        }

        root_tree match {
          case EncodingLeaf(_, v) => Map(v -> Seq(One)) // gère les tree à une seulement  feuilles
          case EncodingNode(_, _,_) => visiter(root_tree,Seq()) // gère les tree à 2feuilles et +
        }

      }
      construct_dict(this)
    }
    /** Computes the bit sequence corresponding to a tentative leaf value.
      * @param x value to encode
      * @return the corresponding bit sequence of `x` is a leaf of encoding tree, `None` otherwise
      */
    def encode(x : S) : Option[Seq[Bit]] = {
      // notez que : on aurait pu faire : if this.has()
      // pour vérifier si le symbole est présent dans les feuille de l'arbre
      if (dict_correspondence.contains(x)) {
        dict_correspondence.get(x) // return some(c)
      } else {
        None
      }
    }

    /** Computes the next value corresponding to the beginning of bit sequence (if possible)
      * @param res the bit sequence to decode
      * @return the decoded value and the bit sequence left to be decoded or `None` if current bit sequence does not lead to a leaf in enconding tree
      */
    def decodeOnce(res : Seq[Bit]) : Option[(S, Seq[Bit])] = {
      // fct parcourrir l'arbre récursivement
      @tailrec
      def parcourir_to_get_leaf(current_node : EncodingTree[S],chemin : Seq[Bit]) : Option[(S, Seq[Bit])] = current_node match {
          case EncodingLeaf(_, v) =>
            // si je suis arrivé à une feuille
            // alors si je suis arrivé au bout du chemin
              Some((v,chemin)) // trad + le chemin restant

          case EncodingNode(_, l, r) =>
            // si c'est un noeud
            if (chemin.nonEmpty) {
              chemin.head match {
                case Zero => parcourir_to_get_leaf(l, chemin.slice(1, chemin.length)) // si 0 parcourir à droite
                case One => parcourir_to_get_leaf(r, chemin.slice(1, chemin.length)) // si 1 parcourir à gauche
              }
            }else {
              None
            }
        }

      // le caractère trouvé ou pas ?
      parcourir_to_get_leaf(this,res)
    }

    /** Computes the sequence of values from the sequence of bits
      * @param res the bit sequence to decode
      * @return the sequence of decoded values or `None` otherwise
      */
    def decode(res : Seq[Bit]) : Option[Seq[S]] = {
      // fct récursive qui va parcourir la séquence et decoder à chaque fois
      @tailrec
      def decode_bis(sequence : Seq[Bit],acc_result : Seq[S] = Seq()) : Option[Seq[S]] = {
        if(sequence.nonEmpty ) {
          this.decodeOnce(sequence) match {
            case Some((c,l_seq)) => decode_bis(l_seq,acc_result :+ c)
            case _ => None // renvoie None si
          }
        } else {
          Some(acc_result)
        }
      }
      // si la sequence à res à decoder est bien rempli
      if (res.nonEmpty) {
        decode_bis(res)
      } else { // si la sequence de bit est vide, nous avons fait le choix de la renvoyer (rien toucher)
        Some(Seq())
      }
    }


    /* MISCELLANEOUS */
    /** Mean length of code associated to encoding tree */
    lazy val meanLength : Double = {
      def parcourir_to_get_leaf(node_current : EncodingTree[S]) : Double = node_current match {
        case EncodingLeaf(lbl, v   ) =>
          this.encode(v) match {
            case Some(v_encoded : Seq[Bit]) => v_encoded.length.toDouble * lbl.toDouble
            case _ => 0.0
          }
         // si c'est une feuille
        case EncodingNode(_, l, r) => parcourir_to_get_leaf(l) + parcourir_to_get_leaf(r) // si c'est un noeud
      }
      parcourir_to_get_leaf(this)/this.label.toDouble
    } // TODO

    /** @inheritdoc */
    override def toString : String = this match
     {
       case EncodingLeaf(lbl, v ) => (v, lbl).toString() // si s'est une feuille
       case EncodingNode(lbl, l, r) => s"EncodingNode([$lbl], $l, $r)"  // si s'est un noeud
     }
  }
case class EncodingNode[S](override val label : Int, left : EncodingTree[S], right : EncodingTree[S]) extends EncodingTree[S](label)
case class EncodingLeaf[S](override val label : Int, value : S) extends EncodingTree[S](label)

