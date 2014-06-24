Paftoga
==========

This package will solve AFT model in high-dimensional case. 


emplik3
====
Paftoga may use several functions provided in [emplik]: cran.r-project.org/web/packages/emplik "emplik". After proper profiling, at least in one dimensional case of the hypothesis testing program *el.cen.EM*, the computational speed could be promoted dramaticly using C/C++. One doen't need to rewrite it in pure C/C++ code.

- I provide a `cumsumsurv` which could calculate `rev(cumsum(rev(x)))` in C code.    
- A C-code uniroot funtion is provided.
- There are several hidden "double/tripple" loops in original R code. Now it is in C code.



```{r}
devtools::install('emplik3')
set.seed(65535)
x <- rexp(10000)
d <- as.numeric(runif(10000)>=.5)
tic=proc.time()
re3=emplik3::el.cen.EM(x,d,mu=1)
toc=proc.time()


tic1=proc.time()
re=emplik::el.cen.EM(x,d,mu=1)
toc1=proc.time()

cat('R version',sessionInfo()[[1]]$nickname,'. Use OpenBLAS(MINGW64).\n')
cat('New pack Time')
toc-tic
cat('Old pack Time')
toc1-tic1
cat('Check uniqueness rate(tol=1e-14):\t',100*sum(abs(re$prob-re3$prob)<1e-14)/length(re$prob),'%\n')
```


R version Frisbee Sailing . Use OpenBLAS(MINGW64).

New pack Time

| user | system | elapsed |
|:----:|:------:|:-------:|
| 1.96 | 0.00   | 1.95    |

Old pack Time

| user  | system | elapsed |
|-------|--------|---------|
| 16.76 | 0.00   | 16.77  | 
  
  Check uniqueness rate(tol=1e-14):   100 %
