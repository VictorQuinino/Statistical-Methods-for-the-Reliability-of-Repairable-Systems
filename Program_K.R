library(pracma)
library(numDeriv)
clear()
l1=0
l2=0
m=5
#Data #Change as needed. 
c1<-c(9.197,14.761,19.246,24.597,25.302,30.948,31.602,34.574,41.059,44.114,44.279,46.381,
      54.519, 56.355, 74.44, 77.236, 80.261,82.815,84.772,88.901,93.727,98.149,106.429)
c2<-c(3.073,4.685,5.125,10.867,11.199,12.046,20.275,21.721,24.944,32.604,34.43,36.343,36.899,49.009,49.27,
      51.357, 53.56, 57.321,58.775,62.879,65.651,68.605,71.925,72.362,80.282,89.826,89.911,91.807,96.57,97.373,99.731,
      103.386)
c3<-c(3.814,18.336,20.887,24.792,28.197,30.747,35.269,39.679,43.93,44.995,54.552,58.139,63.358,63.816,65.727,69.452,
      75.205, 75.401,82.888,83.607,93.903,102.791,103.602)
c4<-c(3.108,11.976,14.593,23.326,24.15,25.627,34.732,36.546,37.267,42.407,46.347,48.593,49.13,52.783,56.483,63.891,
      71.913, 72.627,73.248,77.949,80.195,81.409,87.642,87.782,87.82,91.752,101.995,104.54)
c5<-c(3.346,8.527,10.942,22.723,25.885,27.479,32.869,33.975,35.194,37.683,50.615,53.079,54.31,56.651,62.984,64.305,
      67.309, 77.525,85.587,87.362,91.461,92.839,99.475)
lista=list(c1,c2,c3,c4,c5)
#Modify the distribution as needed 
vero_pp <- function(alpha){
  alpha1=exp(alpha[1])
  alpha2=exp(alpha[2])
  s=0
   for (i in (l2+l1+1):(l2+l1+m)){
    t=0
    d=lista[[i]]
    d1=length(d)
    for (i1 in 1:d1){
      s1=dweibull((d[i1]),alpha1,alpha2)/(1-pweibull(t,alpha1,alpha2))
      s=s+log(s1)
      t=d[i1]
    }
    
  }
  
  return(-s)
  
  
  }
#Change as needed.  
P <- optim(c(log(1),log(5)),  # Initial values
  vero_pp,          # Likelihood function
  method = "BFGS",  # Optimization method
  hessian = TRUE,
  control = list(
    maxit = 1000,    # Increase the maximum number of iterations
    reltol = 1e-10,   # Set relative tolerance for convergence
    abstol = 1e-15   # Set absolute tolerance (if needed)
    
    
  )
)

options(digits = 6)
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