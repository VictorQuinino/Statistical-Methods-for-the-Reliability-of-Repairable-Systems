library(pracma)
library(numDeriv)
library(DiscreteWeibull)
clear()

#Data #Change as needed. 
c1<-c(18,22,45,52,66,68,91,98,100,103,105)
c2<-c(11,17,19,26,27,38,47,48,53,78,80,105)
c3<-c(2,9,18,43,61,71,79,79,95,103,105,105)
c4<-c(3,23,47,53,72,90,105)
c5<-c(19,43,51,54,64,65,91,93,104,104,105,105)
c6<-c(7,36,40,51,56,62,65,80,93,99,100,102,105)
c7<-c(28,40,74,77,81,81,95,97,104,105)
c8<-c(4,20,31,45,55,60,61,99,101,104,105)
c9<-c(7,34,34,71,74,77,101,105)
c10<-c(9,47,70,76,105)

lista=list(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10)
Saida=matrix(0,101,1)
#Modify the distribution as needed 
vero_pp <- function(alpha){
  alpha1=exp(alpha[1])
  alpha2=exp(alpha[2])
  s=0
  
  for (i in 1:10){
    t=0
    d=lista[[i]]
    d1=length(d)
    for (i1 in 1:(d1-1)){
      s1=ddweibull((d[i1]),alpha1,alpha2,zero=FALSE)/(1-pdweibull(t,alpha1,alpha2,zero=FALSE))
      s=s+log(s1)
      t=d[i1]
    }
    s1=(1-pdweibull((d[d1]),alpha1,alpha2,zero=FALSE))/(1-pdweibull(t,alpha1,alpha2,zero=FALSE))
    s=s+log(s1)
  }
  
  return(-s)
  
  
  }
#Change as needed.  
P <- optim(c(log(0.995),log(1.61)),  # Initial values
  vero_pp,          # Likelihood function
  method = "BFGS",  # Optimization method
  hessian = TRUE,
  control = list(
    maxit = 50000,    # Increase the maximum number of iterations
    reltol = 1e-40,   # Set relative tolerance for convergence
    abstol = 1e-25   # Set absolute tolerance (if needed)
    
    
  )
)

options(digits = 10)
hessian_value <- P$hessian
sp=(solve(hessian_value))^0.5

LB1=exp(P$par[1] -1.959963985*sp[1])
UB1=exp(P$par[1] +1.959963985*sp[1])
LB2=exp(P$par[2] -1.959963985*sp[4])
UB2=exp(P$par[2] +1.959963985*sp[4])
k=2 #parameters
log_likelihood <- -P$value
AIC_value <- 2 * k - 2 * log_likelihood

cat('hat_alpha=',exp(P$par),"\n")
cat('Standard Error alpha1=', (((exp(P$par[1]))^2)*sp[1]^2)^0.5, "\n")
cat('Confidence Interval for alpha1 95% = [', LB1, '-', UB1, ']', "\n")
cat('Standard Error alpha2=', (((exp(P$par[2]))^2)*sp[4]^2)^0.5, "\n")
cat('Confidence Interval for alpha1 95% = [', LB2, '-', UB2, ']', "\n")
cat('Log-Likelihood=',-P$value,"\n")
cat('AIC=',AIC_value,"\n")
s=0
alpha1=exp(P$par)[1]
alpha2=exp(P$par)[2]
