
N=10000
alpha=2.5
beta=1
invfun=function(n){
  res=beta/((1-n)^(1/alpha))
  return(res)
}
un=runif(N)
pareto=invfun(un)
hist(pareto,breaks=100,freq=FALSE,xlim=c(0,20),xlab="",ylab="", main = "Histogram of the Pareto approximation")
dens=function(x){
  alpha*beta^(alpha +1)/(x^(alpha+1))*(x>beta)
}
####
curve(dens(x),from=0,to=20,col="red",add=TRUE,n=1000)
simul=pareto
I_hat=mean(pareto)
I_true=alpha/(alpha-1)
runningmeans=cumsum(simul)/(1:N)
runningmeans[N]
I_hat
runningmeans[N]==I_hat
plot(1:N,runningmeans,type="l",col="red",xlab="N", main = "Approaching of the value of I")
abline(h=I_true,col="blue")

###
alpha=2.5
beta=1
var=(alpha*beta^2)/((alpha-2)*(alpha-1)^2)
i=1
while(TRUE){
  if (2*sqrt(var/i)<0.01){
    print(i)
    break
  }
  i = i+1
}  


set.seed(seed=13)
N=1000




