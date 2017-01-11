val a = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))

a(0).indices
val row = 1
val col = 1

(a.indices contains row) &&
(a(row).indices contains col) &&
  a(row)(col) != '-'

a(a.indexWhere(x => x contains 'T')).indexOf('T')