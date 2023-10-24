nonorth_scorespls <- function(x, y, A) {
  y0 <- y
  w <-t <- b <- NULL
  
  for (a in 1:A) {
    v <- t(x) %*% y
    w <- cbind(w, v / c(sqrt(t(v) %*% v)))
    t <- cbind(t, x %*% w[, a])
    qA <- solve(crossprod(t)) %*% t(t) %*% y0
    b <- cbind(b, w %*% qA)
    x <- x - t[, a] %*% t(w[, a])
    y <- y0 - t %*% qA
  }
  return(b[,A])
}