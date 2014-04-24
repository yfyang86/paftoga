pga <-
function(Y,X,ITER=10000,lambda=.01,trace=F,status=F){
if (ITER<=100) {ITER=100;cat('Make ITER=100\n');}
p=dim(X)[2]
n=dim(X)[1]

current.status=simple.lm(Y,rep(1,n))
current.status$beta=rep(0,p)
iter=1;
cent.X(X) -> X.cent

if (!trace){
		if(status)pb <- txtProgressBar(min = 0, max = 100, style=3)
		#while (!(sum(abs(current.status$residual))<1E-10&(iter>1000))){
		while(iter<ITER){
		residual.current		= current.status$residual
		found.j					= which.max(abs(corr.XY(X.cent,residual.current,2)));
		betaj					= sum(residual.current*X[,found.j])/sum(X[,found.j]^2)
		current.status$beta		= current.status$beta+lambda*zerones(p,found.j)*betaj
		current.status$residual	= residual.current-betaj*X[,found.j]*lambda
		iter=iter+1;
		if(status)setTxtProgressBar(pb, round((iter/ITER)*100))
		}
		if(status)close(pb)
		current.status[[4]]=NA;
		names(current.status)[4]='BETARECORD'
		return(current.status);
}else{	sep.p<-floor(ITER/100)
		recordbeta <- matrix(0,nrow=100,ncol=p);ii=1;
		#if(status)pb <- winProgressBar(title="PGA progress bar", label="0% done", min=0, max=100, initial=0)
		if(status)pb <- txtProgressBar(min = 0, max = 100, style=3)
		#while (!(sum(abs(current.status$residual))<1E-10&(iter>1000))){
		while(iter<ITER){
		residual.current		= as.double(current.status$residual)
		found.j					= which.max(abs(corr.XY(X.cent,residual.current,2)));
		betaj					= sum(residual.current*X[,found.j])/sum(X[,found.j]^2)
		current.status$beta		= current.status$beta+lambda*zerones(p,found.j)*betaj
		current.status$residual	= residual.current-betaj*X[,found.j]*lambda
		iter=iter+1;
		if(status) setTxtProgressBar(pb, round((iter/ITER)*100))
		#setWinProgressBar(pb, iter/(ITER)*100, label=sprintf("%d%% done", round((iter/ITER)*100)))
		if(iter%%sep.p==0) {recordbeta[ii,]=current.status$beta;ii=ii+1}
		}
		if(status)close(pb)
		######################################
		cols=rainbow(p)
		plot(1:100,recordbeta[,1],lty=2,col=cols[1],ylim=1.5*range(recordbeta),type='l');
		for (i in 2:p){
			points(1:100,recordbeta[,i],lty=2,col=cols[i],ylim=1.5*range(recordbeta),type='l');
		}

		current.status[[4]]=t(recordbeta);
		names(current.status)[4]='BETARECORD'
		return(current.status);	
			}
}
