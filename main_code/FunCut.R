## Example
# s <- 2
# x <- unique(sort(ceiling(runif(50)*50)))
# z <- FunCut(x)
# x
# z

FunCut <- function(x, s = 2){
  n <- length(x)
  x1 <- x[-n]
  x2 <- x[-1]
  x.dif <- x2 - x1
  k <- which(x.dif > s)
  kn <- c(k, n) - c(0, k) 
  z <- rep(1:(length(k) + 1), kn)
  return(z)
}



