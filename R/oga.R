oga <-
function(Y,X,k=-10,trace=F,status=F){

p=dim(X)[2]
n=dim(X)[1]
minpn=qr(X)$rank
if (p>=n) minpn=minpn-1;
if (k>0){
	k=ceiling(k);
	minpn=min(k,minpn)
	}
current.status=simple.lm(Y,rep(1,n))
current.status$beta=rep(0,p)

selected.idx<-NULL;
#if(status)pb <- winProgressBar(title="OGA progress bar", label="0% done", min=0, max=100, initial=0)
if(status)pb <- txtProgressBar(min = 0, max = 100, style=3)
for (i in 1:minpn){#
	residual.current		= as.double(current.status$residual)
	if(i!=1){	
		#found.j					= (1:p)[-selected.idx][which.min(abs(l2.res(XX[,-selected.idx],residual.current)))];
		#found.j=(1:p)[-selected.idx][ which.max(abs(corr.XY(X.cent[,-selected.idx],residual.current,2)))];
		found.j				= which.max(abs(corr.XY(X,residual.current,2)));
		}else{
		#found.j					= which.min(l2.res(X,residual.current));
		found.j= which.max(abs(corr.XY(X,residual.current,2)));
	}
	
	#if (found.j%in%selected.idx) break;

	if (i>1){
			#Xj=projection.mat(X[,found.j],X[,selected.idx])$orth
			Xj=projection.mat2(X[,found.j],X[,selected.idx])	
		}else{
			Xj=X[,found.j]
	}					
	betaj					= sum(residual.current*Xj)/sum(Xj^2)
	current.status$beta		= current.status$beta+zerones(p,found.j)*betaj
	current.status$residual	= residual.current-betaj*Xj
	selected.idx			= c(selected.idx,found.j)
	if (trace){
		cat('Selected:\t',found.j,'\n')
		}
	#if(status)setWinProgressBar(pb, i/p*100, label=sprintf("%d%% done", round((i/p)*100)))	
	if(status) setTxtProgressBar(pb, round((i/minpn)*100))
}
current.status[[4]]			= NA;
current.status[[5]]			= selected.idx;
names(current.status)[4:5]	= c('BETARECORD',"Selected Index");
if(status)close(pb)
if(trace){
	plot(current.status$beta,xlab='locs',ylab='Betas',type='h',col=rainbow(p));
	}
return(current.status);
}
