corr.XY <-
function(X,y,mod=1){# y is an vector.################ABS
	if(mod==1) return((colSums(cent.X(X)*y)));
	if (mod==2) return (colSums(as.matrix(X*y))/colSums(as.matrix(X^2)));
}
