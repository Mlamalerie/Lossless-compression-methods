package compress.statistic

import compress.Compressor

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
      -source.toList.map(c => occurrences(c).toDouble/source.length * log2(occurrences(c).toDouble/source.length)).sum
    }

    /** The sequence of occurrences sorted by count */
    val orderedCounts : Seq[(S, Int)] = {
      occurrences.toList.sortWith(_._2 > _._2)
    }

    /** The encoding tree (in most cases, depends from `source`) */
    def tree : Option[EncodingTree[S]]

    /** @inheritdoc */
    def compress(msg: Seq[S]): Seq[Bit] = {
      Seq.empty[Bit]
    }

    /** @inheritdoc */
    def uncompress(res: Seq[Bit]): Option[Seq[S]] = {
      None
    }
  }
