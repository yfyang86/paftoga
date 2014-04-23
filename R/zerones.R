zerones <-
function(n,loc=NA){
	re=rep(0,n);
	if (is.na(loc))return(re);
	re[loc]=1;
	re
	}
