projection.mat2 <-
function(X,LinearSys){
	Y=as.matrix(LinearSys)
	as.double(lm(X~Y-1)$residual)
	}
