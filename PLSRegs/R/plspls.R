plspls <- function(x, y, A){
  V <- t(x) %*% y
  arg<-x%*%V
  b <- V %*% solve(crossprod(arg))%*%t(arg)%*%y
  
  
  for(a in 2:A){
    V <- cbind(b, t(x) %*% (x %*% b[, a-1]))
    arg<- x%*%V
    b <- cbind(b, V %*% solve(crossprod(arg))%*%t(arg)%*%y)
  }
  return(b[,A])
}
