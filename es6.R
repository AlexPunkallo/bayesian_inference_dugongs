
n=100

sequence=sample(c(0,1,2), size=n, replace=T, prob=c(0.25,0.35,0.4))
?hist
hist(sequence,breaks=100,probability=T)

un=runif(n)
un

invfun <- function(u){
 if (u<=0.25) {
   return(0)}
 
  else if (u<=0.6 && u>0.25){
    return(1)}
  
  else if (u<=1 && u>0.6){
    return(2)}
 }

vectfun=Vectorize(invfun)
vectfun(un)


