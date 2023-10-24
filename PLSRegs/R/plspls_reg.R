plspls_reg <- function(y, x, n_comp = NULL, nfold = 5, comp_max = 10, cv = TRUE){
  arg <- "plspls"
  
  if(cv == TRUE){
    opt.A <- kfoldCV(y, x, nfold, comp_max, method = arg)
    coef <- plspls(x, y, A = opt.A)
    fits <- x %*% coef
    err <- y - fits
  }else{
    coef <- plspls(x, y, A = n_comp)
    fits <- x %*% coef
    err <- y - fits
  }
  
  return(list(coef = coef, fitted.values = fits, residuals = err))
}

