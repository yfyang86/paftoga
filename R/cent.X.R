cent.X <-
function(X){
	X<-as.matrix(X)
	mean.X	=	colMeans(X)
	cov.X   = rowSums((t(X)-mean.X)^2)
	t((t(X)-mean.X)/sqrt(cov.X))
}
