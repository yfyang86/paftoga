projection.mat <-
function(X,LinearSys,mod=1){
	Y=as.matrix(LinearSys)
	YY=matrix(0,nrow=nrow(Y),ncol=ncol(Y))
	if (mod==1) {Y.qr=qr(Y);Y=qr.R(Y.qr)[,1:(Y.qr$rank)];}
	YY[1:(Y.qr$rank),1:(Y.qr$rank)]=Y
	tmp=YY%*%solve(t(YY)%*%YY)%*%t(YY)%*%X
	list(projection=tmp,orth=X-tmp)#lm(X~Y)$residual...
}
