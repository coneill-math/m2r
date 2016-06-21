#' for this to work, you need to have modified your init-PHCpack.m2 file
#' instead of changing your .bashrc file to establish the path of phc

solve_system <- function (mpolyList, ring) {
  m2("needsPackage \"PHCpack\"")
  m2("R = CC[x,y]")
  m2("a = solveSystem {x+y^2-4*x*y,x^3+5*x*y+y}")
  m2("for i in 0..(#a-1) list (a#i#Coordinates)")
}
