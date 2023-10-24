directscorespls <- function(x, y, A) {
  s <- t(x) %*% y
  utemp <- x %*% s
  nrm <- norm(utemp, type = "2")
  R <- as.matrix(s/nrm)
  U <- as.matrix(utemp/nrm)
  P <- t(x) %*% U
  q <- t(y) %*% U
  b <- R %*% t(q)
  for (a in 2:A) {
    s <- s - P[, a-1] %*% t(q[, a-1])
    rtemp <- s - R %*% (t(P) %*% s)
    utemp <- x %*% rtemp
    nrm <- norm(utemp, type = "2")
    R <- cbind(R, rtemp/nrm)
    U <- cbind(U, utemp/nrm)
    P <- cbind(P, t(x) %*% U[, a])
    q <- cbind(q, t(y) %*% U[, a])
    b <- cbind(b, R %*% t(q))
  }
  return(b[,A])
}