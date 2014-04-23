norm.lp <-
function(x,lp=2){
	if (lp<0) stop('p>0');
	if (lp==0) return(sum(abs(x)>10*.Machine$double.eps));
	sum(x^lp)^(1/lp)
}
