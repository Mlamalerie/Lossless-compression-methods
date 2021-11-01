import compress.statistic.{Bit, Zero,One}
import scala.math
print(One)

//val occurrences :  = () =>
def occurrences[S](mot : Seq[S]) : Map[S, Int] = {
	 mot.groupBy(l => l).map(t => (t._1, t._2.length))
}
def orderedCounts[S](mot : Seq[S]) : Seq[(S, Int)] = {
	val map_occurences = occurrences(mot)
	map_occurences.toList.sortWith(_._2 > _._2)
}




def entropy[S](mot : Seq[S]) : Double = {
	val log2 = (x: Double)  => scala.math.log(x) / scala.math.log(2)
	val map_occurences = occurrences(mot) // ici les for k,v et bah v > 0

	print(mot.toList.map(c => map_occurences(c).toDouble/mot.length * log2(map_occurences(c).toDouble/mot.length)))
	-mot.toList.map(c => map_occurences(c).toDouble/mot.length * log2(map_occurences(c).toDouble/mot.length)).sum
}
val mot = Seq(Zero,One,Zero)

//var mot = "m1m"
occurrences(mot)
orderedCounts(mot)
entropy(mot)


