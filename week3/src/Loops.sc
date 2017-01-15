def power (x: Double, exp: Int): Double = {
  var r = 1.0
  var i = exp
  while (i > 0) { r = r * x; i = i - 1 }
  r
}

power(2.5, 3)

for (i <- 1 until 3; j <- "abc") println (i + " " + j)