

x = c( 1.0,  1.5,  1.5,  1.5, 2.5,   4.0,  5.0,  5.0,  7.0,
        8.0,  8.5,  9.0,  9.5, 9.5,  10.0, 12.0, 12.0, 13.0,
        13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5)
y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
        2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
        2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57)

loglik = function(theta){ 
  alpha = theta[1]
  beta = theta[2]
  gamma = theta[3]
  tau = theta[4] 
  mu=alpha-beta*gamma^x
  lik = dnorm(y,mu, tau,log=T)
  return(sum(lik))
#-sum(log(lik))
}

#thetas=optim(c(2,1,0.5,0.1),loglik,control=list(fnscale=-1))
thetas=optim(c(1.5,2,0.3,1.2),loglik,control=list(fnscale=-1))
vv=thetas$par
vv

plot(x,y,col="blue",main="Dugongs likelihood")
lines(x,vv[1]-vv[2]*vv[3]^x,type="l",col="red")

########

pialfa=function(alfa){
  val=dnorm(alfa,0,10000,log=T)
  return(val)
}
pibeta=function(beta){
  val=dnorm(beta,0,10000,log=T)
  return(val)
}
pigamma=function(gamma){
  val=dunif(gamma,0,1)
  return(log(val))
}
pitau=function(tau){
  val=densigamma(tau^2,0.001,0.001)
  return(log(val))
}

LLP=function(vec_param){
  alpha=vec_param[1]
  beta=vec_param[2]
  gamma=vec_param[3]
  tau=vec_param[4]
  mu=alpha-beta*gamma^x
  lik=sum(dnorm(y,mu,tau,log=T))
  
  return(lik+pialfa(alpha)+pitau(tau)+pibeta(beta)+pigamma(gamma))
}

thetas=optim(c(1.5,2,0.7,1),LLP, control=list(fnscale=-1))
res=thetas$par
res

lines(x,res[1]-res[2]*res[3]^x,type="l",col="green",add=TRUE)


