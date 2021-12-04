import compress.lz.{Dictionaries, LZW}
type Dictionary = IndexedSeq[String]
val msg = "ababbabcababba"
val init_dict = IndexedSeq("a", "b", "c")

val r = new LZW()

r.compress("belle echelle")
