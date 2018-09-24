rel=(dnorm(x,0,1)/dcauchy(x,0,1))
m=max(rel) #it makes sqrt(2pi/e)
N=10000
x = rcauchy(N,0,1)
numb = c()
val= c()
for(i in 1:length(x)){
  U = runif(n = 1, min = 0, max = 1)
  if(dcauchy(x[i], 0, 1)*m*U <= dnorm(x[i], 0, 1)) {
    val[i]=x[i]
  }
  else if(dcauchy(x[i],0,1)*m*U > dnorm(x[i], 0, 1)) {
  }
}
val2=val[!is.na(val)]
numb=length(val2)
acc=abs((numb / N)-(1/m))
acc

curve(dnorm(x,0,1),col="red",xlim=c(-4,4),ylim = c(0,0.5),ylab="g(x)",main="Histogram of the approximation")
curve(dcauchy(x,0,1)*m,col="blue",xlim=c(-4,4),add=T)
hist(val2,freq=F,breaks=50,add=TRUE)