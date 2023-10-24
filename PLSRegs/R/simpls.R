simpls <- function(x, y, A) {
  x <- as.matrix(x)
  I <- dim(x)[1]
  K <- dim(x)[2]
  
  V <- matrix(0, nrow = K, ncol = A)
  R <- matrix(0, nrow = K, ncol = A)
  T <- matrix(0, nrow = I, ncol = A)
  P <- matrix(0, nrow = K, ncol = A)
  Q <- numeric()
  U <- matrix(0, nrow = I, ncol = A)
  
  s <- t(x) %*% y
  for (a in 1:A) {
    r <- s
    t <- x %*% r
    tt <- norm(t, type = "2")
    t <- t / tt
    r <- r / tt
    p <- t(x) %*% t
    q <- t(y) %*% t
    u <- y %*% q
    v <- p
    if (a > 1) {
      v <- v - V %*% t(V) %*% p
      u <- u - T %*% t(T) %*% u
    }
    v <- v / norm(v, type = "2")
    s <- s - v %*% t(v) %*% s
    R[, a] <- r
    T[, a] <- t
    P[, a] <- p
    Q[a] <- q
    U[, a] <- u
    V[, a] <- v
  }
  b <- apply(R %*% diag(Q), 1, cumsum)
  return(b[A,])
}
