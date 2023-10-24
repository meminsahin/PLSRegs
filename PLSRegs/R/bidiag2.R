bidiag2 = function(X, Y, a){
  B = matrix(0, nrow = a, ncol = 2)
  w = t(X) %*% Y
  w = w / norm(w, type = "2")
  W = w
  t = X %*% w
  rho = norm(t, type = "2")
  t = t / rho
  T = t
  B[1,1] = rho
  d = w / rho
  beta = d %*% (t(t) %*% Y)
  for(i in 2:a){
    w = t(X) %*% t - rho * w
    w = w - W %*% (t(W) %*% w)
    theta = norm(w, type = "2")
    w = w / theta
    W = cbind(W, w)
    t = X %*% w - theta * t
    t = t - T %*% (t(T) %*% t)
    rho = norm(t, type = "2")
    t = t / rho
    T = cbind(T, t)
    B[(i-1),2] = theta
    B[i,1] = rho
    d = (w - theta * d) / rho
    beta = cbind(beta, beta[,(i-1)] + d %*% (t(t) %*% Y))
  }
  return(beta[,a])
}