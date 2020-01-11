fun <T> everyNth(lst: List<T>, n: Int): List<T> {
	var idx = n-1
	val res = ArrayList<T>()
	while (idx < lst.size) {
		res.add(lst.get(idx))
		idx += n
	}
	// immutable
	val nres: List<T> = res
	return nres
}

fun main() {
	val test1 = listOf(1, 2, 3, 4, 5, 6, 7)
	val test2 = listOf("a", "b", "c", "d", "e", "f", "g", "h", "i")
	val test3 = listOf(false, true, false, true, false, true)

	val output1 = everyNth(test1, 2)
	val output2 = everyNth(test2, 2)
	val output3 = everyNth(test3, 2)

	println(output1)
	println(output2)
	println(output3)
}
