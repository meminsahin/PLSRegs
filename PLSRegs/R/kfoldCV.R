
kfoldCV <- function(y, x, nfold = 5, comp_max = 10,
                    method = c("bidiag2", "bidiag1", "plspls", "plsf", "krylovpls",
                               "directscorespls", "simpls", "nonorth_scorespls")){

  method <- match.arg(method)

  x <- as.matrix(x)
  y <- as.matrix(y)
  n <- dim(y)[1]
  p <- dim(x)[2]
  oIndx <- 1:n
  samp_dat <- oIndx[sample(n)]
  kfold <- nfold
  A_max <- comp_max
  folds <- cut(seq(1,n), breaks = kfold, labels=FALSE)


  nfMspe <- numeric()

  for(i in 1: (A_max-1)){

    mspe_j <- numeric()
    for(j in 1:kfold){
      indx_test <- which(folds == j, arr.ind = TRUE)

      Y_tr <- y[-indx_test]
      Y_te <- y[indx_test]

      X_tr <- x[-indx_test, ]
      X_te <- x[indx_test, ]

      if(method == "bidiag2")
        coef <- bidiag2(X_tr, Y_tr, i+1)
      if(method == "bidiag1")
        coef <- bidiag1(X_tr, Y_tr, i+1)
      if(method == "plspls")
        coef <- plspls(X_tr, Y_tr, i+1)
      if(method == "plsf")
        coef <- plsf(X_tr, Y_tr, i+1)
      if(method == "krylovpls")
        coef <- krylovpls(X_tr, Y_tr, i+1)
      if(method == "directscorespls")
        coef <- directscorespls(X_tr, Y_tr, i+1)
      if(method == "simpls")
        coef <- simpls(X_tr, Y_tr, i+1)
      if(method == "nonorth_scorespls")
        coef <- nonorth_scorespls(X_tr, Y_tr, i+1)

      predicted_values <- X_te %*% coef
      mspe_j[j] <- mean((Y_te - predicted_values)^2)
    }

    nfMspe[i] <- mean(mspe_j)

  }

  diff <- nfMspe[-(A_max-1)] - nfMspe[-1]

  opt.A <- which(diff < 0.01)[1]+1

  return(opt.A)
}

