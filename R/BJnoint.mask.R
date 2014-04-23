BJnoint.mask<-function (x, y, delta, beta0 = NA, maxiter = 30, error = 1e-05){
iter <- function(x, y, delta, beta){
  N <- length(delta)
  u <- x %*% beta
  res <- y - u
  niceorder <- order(res, - delta) # order the obs according to
  resorder <- res[niceorder]     # res, if tie then according to
  dorder <- delta[niceorder]  # delta value i.e. d=1 comes first
  dorder[N] <- 1              # added 2005 3/20
  uorder <- u[niceorder]
  ystar <- y[niceorder]  # should I just let ystar <- delta ?
  xorder <- as.matrix(x[niceorder,])
  temp <- WKM(x=resorder, d=dorder, zc=1:N)  # add  ( , zc=1:N )  2005, 3
  jifen <- rev( cumsum( rev(resorder * temp$jump)) )
  Sresorder <- temp$surv
  for (i in 1:N) if (dorder[i] == 0) {
    ystar[i] <- uorder[i] + jifen[i]/Sresorder[i]
  }
  return( list(coef=lm( ystar ~ 0 + xorder )$coef,residual=ystar-uorder) );
}
  x <- as.matrix(x)
  newtemp <- matrix(NA, ncol = ncol(x), nrow = 3)
  newtemp[1, ] <- beta0
  if (any(is.na(beta0)))newtemp[1, ] <- lm(y ~ 0 + x)$coef
  for (i in 2:3) {
    newtemp[i, ] <- iter(x, y, delta, newtemp[i - 1, ])$coef
  }
  num <- 2
  while (num <= maxiter && error <= sum(abs(newtemp[2, ] -
                                              newtemp[3, ]))) {
    newtemp[2, ] <- newtemp[3, ]
    newtemp[3, ] <- iter(x, y, delta, newtemp[2, ])$coef
    num <- num + 1
  }
  if (num > maxiter) {
    newtemp[3, ] <- (newtemp[2, ] + newtemp[3, ])/2
  }
  beta= as.double(newtemp[3, ]);
  residual=iter(x, y, delta, newtemp[3, ])$residual
  return(list(beta =  beta, iteration = num,residual=residual));
}

