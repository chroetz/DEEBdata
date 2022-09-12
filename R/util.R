numberOfTermsInPoly <- \(polyDeg, d) {
  sum(sapply(0:polyDeg, \(deg) choose(d+deg-1, deg)))
}
