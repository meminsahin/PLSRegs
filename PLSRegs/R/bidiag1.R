bidiag1 = function(X, Y, a){
  beta = NULL
  B = matrix(0, nrow = a, ncol = 2)
  gamma = norm(Y, type = "2")
  t = Y / gamma
  T = t
  w = t(X) %*% t
  alpha = norm(w, type = "2")
  w = w / alpha
  W = w
  d = w
  reg = 0
  rhobar = alpha
  phibar = gamma

  for(i in 1:a){
    t = X %*% w - alpha * t
    t = t - T %*% (t(T) %*% t)
    gamma = norm(t, type = "2")
    t = t /gamma
    T = cbind(T, t)
    w = t(X) %*% t - gamma * w
    w = w - W %*% (t(W) %*% w)
    alpha = norm(w, type = "2")
    w = w / alpha
    W = cbind(W, w)
    rho = norm(c(rhobar,gamma), type = "2")
    cos = rhobar / rho
    sin = gamma / rho
    theta = sin * alpha
    rhobar = -cos * alpha
    phi = cos * phibar
    phibar = sin * phibar
    G = matrix(c(cos, sin,sin,-cos), nrow = 2, ncol = 2, byrow = TRUE)
    T[,(i:(i+1))] = T[,(i:(i+1))] %*% G
    B[(i-1), 2] = theta
    B[i, 1] = rho
    reg = reg + d * (phi / rho)
    beta = cbind(beta, reg)
    d = w - d * (theta / rho)
  }
  return(beta[,a])
}
