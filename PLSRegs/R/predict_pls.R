predict_pls <- function(object, x_new){
  coef <- object$coef
  preds <- x_new %*% coef
  return(preds)
}