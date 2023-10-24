plsf <- function(x, y, A) {
  W <- t(x) %*% y
  nw1 <- norm(as.matrix(W), type = "2")
  W <- W / nw1
  U <- x %*% W
  Rd <- numeric(A)
  Rd[1] <- norm(U, type = "2")
  U <- U / Rd[1]
  Uy <- numeric(A)
  Uy[1] <- t(U) %*% y
  Ru <- numeric(A)
  i <- 1
  while (i > 0) {
    i <- i + 1
    W <- cbind(W, t(x) %*% U - W[, i-1] * Rd[i-1])
    Ru[i-1] <- norm(as.matrix(W[, i]), type = "2")
    W[, i] <- W[, i] / Ru[i-1]
    if (i > A) {
      break
    } else {
      U <- x %*% W[, i] - U * Ru[i-1]
      Rd[i] <- norm(U, type = "2")
      U <- U / Rd[i]
      Uy[i] <- t(U) %*% y
    }
  }
  W <- W[, 1:(i-1)]
  Ru <- Ru[-(i-1)]
  n1 <- i - 1
  if(length(Ru) == 1){
    R <- diag(Rd) + rbind(c(0,Ru),0)
  }else{
    R <- diag(Rd) + rbind(cbind(0,diag(Ru)),0)
  }
  
  Uy <- t(Uy)
  invR <- solve(R)
  b <- matrix(0, nrow = ncol(x), ncol = A)
  for (i in 1:A) {
    b[, i] <- W[, 1:i] %*% (invR[1:i, 1:i] %*% Uy[1:i])
  }
  return(b[,i])
}