package compress.statistic

import compress.Compressor

/** A statistic compressor relies on the statistics of symbols in source
  * @param source the input source
  * @tparam S type of symbol used in source
  */
abstract class StatisticCompressor[S](source : Seq[S]) extends Compressor[S, Seq[Bit]]
  {
    /** A map giving the occurrences of each symbol in the source sequence */
    val occurrences : Map[S, Int] = ???

    /** SHANNON entropy of source */
    val entropy : Double = ???

    /** The sequence of occurrences sorted by count */
    val orderedCounts : Seq[(S, Int)] = ???

    /** The encoding tree (in most cases, depends from `source`) */
    def tree : Option[EncodingTree[S]]

    /** @inheritdoc */
    def compress(msg: Seq[S]): Seq[Bit] = ???

    /** @inheritdoc */
    def uncompress(res: Seq[Bit]): Option[Seq[S]] = ???
  }
