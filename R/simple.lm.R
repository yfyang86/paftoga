simple.lm <-
function(Y,X){
	re=lm(Y~X)
	list(alpha=as.numeric( coef(re)[1]),beta=as.numeric(coef(re)[-1]),residual=as.numeric(re$residuals))
}
