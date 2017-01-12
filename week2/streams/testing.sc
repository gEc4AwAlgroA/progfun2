val a = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))

a(0).indices
val row = 1
val col = 1

(a.indices contains row) &&
(a(row).indices contains col) &&
  a(row)(col) != '-'

a(a.indexWhere(x => x contains 'T')).indexOf('T')

val b = (for {
  i <- 0 to 5
} yield (i, List(i, 2*i))).toList

b.length
b.take(1)

val c = List()
c.take(1)

(b map (_._2.length)).min

if (true) 1 else 0
