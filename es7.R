
N=100000
m = max(dbeta(x,3,3))

x = runif(N,0,1)
numb = c()
val= c()

for(i in 1:length(x)){
  U = runif(n = 1, min =  0, max = 1)
  if(dunif(x[i], 0, 1)*m*U <= dbeta(x[i], 3, 3)) {
    val[i]=x[i]
    #numb[i] = 1
  }
  else if(dunif(x[i],0,1)*m*U > dbeta(x[i], 3, 3)) {
    #numb[i] = 0
  }
}

val2=val[!is.na(val)]
numb=length(val2)
acc=abs((numb / N)-(1/m))
acc

curve(dbeta(x,3,3),col="red", ylim= c(0,2),ylab="g(x)",main="Histogram of the approximation");
abline(h = m, col = 'blue')
hist(val2,freq=F,breaks=50,add=TRUE)
#  as we espected the frequency of the accepted values 
# is close to 1/k
# indeed we can define 1/k as the acceptance probability