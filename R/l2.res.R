l2.res <-
function(X,residual.current){
	beta = as.numeric( colSums(as.matrix(residual.current*X))/colSums(as.matrix(X)^2))
	return (colSums((t(t(X)*beta)-residual.current)^2));
}
