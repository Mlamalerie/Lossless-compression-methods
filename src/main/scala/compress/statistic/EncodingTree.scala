package compress.statistic

/** Trait for binary encoding trees (integer-labeled binary trees)
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
    lazy val dict_correspondence : Map[S,Seq[Bit]] = {
      def construct_dict(tree : EncodingTree[S]) : Map[S,Seq[Bit]] = {
        def visiter(node : EncodingTree[S], bits_list : Seq[Bit]) : Map[S,Seq[Bit]] = {
          node match {
            case EncodingLeaf(_, v) => Map[S,Seq[Bit]]()  + (v -> bits_list) // si c'est une feuille
            case EncodingNode(_, l, r) => visiter(l, bits_list :+ Zero) ++ visiter(r, bits_list :+ One) // si c'est un noeud
          }
        }
        visiter(tree,Seq())
      }
      construct_dict(this)
    }
    /** Computes the bit sequence corresponding to a tentative leaf value.
      * @param x value to encode
      * @return the corresponding bit sequence of `x` is a leaf of encoding tree, `None` otherwise
      */
    def encode(x : S) : Option[Seq[Bit]] = {
      if (dict_correspondence.contains(x)) {
        dict_correspondence.get(x)
      } else {
        None
      }
    }

    /** Computes the next value corresponding to the beginning of bit sequence (if possible)
      * @param res the bit sequence to decode
      * @return the decoded value and the bit sequence left to be decoded or `None` if current bit sequence does not lead to a leaf in enconding tree
      */
    def decodeOnce(res : Seq[Bit]) : Option[(S, Seq[Bit])] = {
      def parcours(current_node : EncodingTree[S],chemin : Seq[Bit]) : Option[S] = {
        //println(chemin.length)
        current_node match {
          case EncodingLeaf(_, v) => {
            // si je suis arrivé à une feuille
            // alors si je suis arrivé au bout du chemin
            if(chemin.length == 0) {
              Some(v)
            } else {
              None
            }
          }
          case EncodingNode(_, l, r) => {
            if (chemin.length > 0) {
              chemin(0) match {
                case Zero => parcours(l, chemin.slice(1, chemin.length))
                case One => parcours(r, chemin.slice(1, chemin.length))
              }
            }else {
              None
            }
          } // si c'est un noeud
        }
      }
      // le caractère trouvé ou pas ?
      val c = parcours(this,res)
      if (c.isDefined) { // si oui
        Some((c.get,res))
      } else { //  sinon
        None
      }
    }

    /** Computes the sequence of values from the sequence of bits
      * @param res the bit sequence to decode
      * @return the sequence of decoded values or `None` otherwise
      */
    def decode(res : Seq[Bit]) : Option[Seq[S]] = {
      // return (le char, nombre de saut)
      def tree_find_char(current_node: EncodingTree[S], sequence: Seq[Bit], prof: Int = 0): Option[(S, Int)] = {
        //println(sequence.length)
        current_node match {
          // si je suis arrivé à une feuille
          case EncodingLeaf(_, v) => Some((v, prof))

          case EncodingNode(_, l, r) => {
            if (sequence.length > 0) {
              sequence(0) match {
                case Zero => tree_find_char(l, sequence.slice(1, sequence.length), prof + 1)
                case One => tree_find_char(r, sequence.slice(1, sequence.length), prof + 1)
              }
            } else {
              None
            }

          } // si c'est un noeud
        }
      }

      def all_run(sequence: Seq[Bit],acc_seq_result : Seq[S] = Seq.empty[S]) : Option[Seq[S]] = {
        // si il reste encore du chemin à faire

        if (sequence.length > 0) {
          //println("*")
          val char_and_cursor = tree_find_char(this, sequence)
          //println("***")
          // si la lettre est trouvé
          if (char_and_cursor.isDefined) { // si oui
            //println("**")
            val le_char = char_and_cursor.get._1
            val le_cursor = char_and_cursor.get._2
            all_run(sequence.slice(le_cursor,sequence.length),acc_seq_result :+ le_char )

          } // si oh chakal ya pas la lettre dans mon arbre la elle existe pas
          else {
            None
          }
        } else {
          if(acc_seq_result.isEmpty) {
            None
          } else {
            Some(acc_seq_result)
          }
        }
      }
      // le caractère trouvé ou pas ?
      all_run(res) // TODO faire des rise error selon si ya une lettre qui est pas reconnu

    }


    /* MISCELLANEOUS */

    /** Mean length of code associated to encoding tree */
    lazy val meanLength : Double = {
      def parcourir(node_current : EncodingTree[S]) : Double = node_current match {
        case EncodingLeaf(lbl, v   ) => {
          val v_encoded = this.encode(v)
          if(v_encoded.isDefined) {
            v_encoded.get.length.toDouble * lbl.toDouble
          } else {
            0.0
          }
        } // si c'est une feuille
        case EncodingNode(_, l, r) => parcourir(l) + parcourir(r) // si c'est un noeud
      }
      parcourir(this)/this.label.toDouble
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

