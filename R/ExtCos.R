library(minpack.lm)

fn.res1 <- function(par, tmp.dat) {
  tmp.dat[, 2] - (par[1] + par[2] * cos((tmp.dat[, 1] - par[3]) * 2 * pi / 24))
}

fn.res2 <- function(par, tmp.dat) {
  ct <- cos((tmp.dat[, 1] - par[5]) * 2 * pi / 24)
  lct <- exp(par[4] * (ct - par[3])) / (1 + exp(par[4] * (ct - par[3])))
  rt <- par[1] + par[2] * lct
  tmp.dat[, 2] - rt
}

ExtCos <- function(
  x,
  logtransform = TRUE){
  
  if(logtransform){
    x = log(x + 1)
  }
  n.days <- length(x)/1440
  tmp.dat <- data.frame(time = rep(1:1440, n.days) / 60, Y = x)
  #fit <- cosinor.lm(Y ~ time(time) + 1, data = tmp.dat, period = 24)
  #####stage 1#####
  par <- c(1, 1, 1)
  fit.nls <- nls.lm(par = par, fn = fn.res1, tmp.dat = tmp.dat)
  coef.nls <- coef(fit.nls)
  #####stage 2#######
  newpar <- c(coef.nls[1] - abs(coef.nls[2]), 2, 0, 2, coef.nls[3])
  fit2.nls <- nls.lm(newpar, fn = fn.res2, tmp.dat = tmp.dat, control = nls.lm.control(maxiter = 1000))
  
  cosinor.stat = as.numeric(coef(fit2.nls))

  
  return(  list(min = cosinor.stat[1],
                amp = abs(cosinor.stat[2]),
                alpha = cosinor.stat[3],
                beta = cosinor.stat[4],
                acro = cosinor.stat[5]))
}