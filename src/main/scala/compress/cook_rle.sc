import compress.RLE

print("p")
def sum(list: List[Int]): Int = list match {
	case Nil => 0
	case n :: rest => println(n, rest); n + sum(rest)
}


val nums = List(1, 2, 3, 4, 5)

val msg_original = "Whaaaaat"

val r = new RLE[Char]()
val msg_compressed = r.compress(msg_original)
val msg_uncompressed = r.uncompress(msg_compressed)

print(" # msg => %s [length=%d]".format(msg_original.toString, msg_original.length))
print(" # compressed() => %s".format(msg_compressed.toString))
print("                   [length=%d]".format(msg_compressed.map(e => e._1.toString.length + e._2.toString.length).sum))
//msg_compressed.map(e => e._1.toString() + e._2).mkString("")
var gain = (msg_original.length - msg_compressed.map(e => e._1.toString.length + e._2.toString.length).sum).toFloat * 100 / msg_original.length

print(" # uncompressed() => %s".format(msg_uncompressed.toString))

// https://exercism.org/tracks/scala/exercises/run-length-encoding/solutions/86051bccf3504cb9960f64eaae63fc9e
