bidiag1_reg <- function(y, x, n_comp = NULL, nfold = 5, comp_max = 10, cv = TRUE){
  arg <- "bidiag1"

  if(cv == TRUE){
    opt.A <- kfoldCV(y, x, nfold, comp_max, method = arg)
    coef <- bidiag1(x, y, a = opt.A)
    fits <- x %*% coef
    err <- y - fits
  }else{
    coef <- bidiag1(x, y, a = n_comp)
    fits <- x %*% coef
    err <- y - fits
  }

  return(list(coef = coef, fitted.values = fits, residuals = err))
}
