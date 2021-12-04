import compress.lz.{Dictionaries, LZW}
type Dictionary = IndexedSeq[String]
val msg = "ababbabcababba"
val init_dict = IndexedSeq("a", "b", "c")

init_dict(5)
val r = new LZW()

r.compress("belle echelle")
