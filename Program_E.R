library(pracma)
library(numDeriv)
clear()
l1=2
l2=4
m=4
#Data #Change as needed. 
c1<-c(6700.14)
c2<-c(6981.70)
c3<-c(708.15, 1087.54, 2632.96, 3795.93, 7650.02, 7936.53, 
      13582.05, 13599.30, 14667.52, 17616.93)
c4<-c(1143.08, 4172.89, 4231.24, 4930.91, 5475.86, 9581.46, 
      10146.97, 12355.46, 14900.22, 16182.83)
c5<-c(2263.72, 7690.81, 9017.68, 9835.47, 10066.96, 11881.84, 
      13731.06, 15937.57, 18639.87, 18707.09)
c6<-c(2000.07, 3285.58, 3575.88, 4071.69, 4882.62, 5174.08, 
      6181.00, 6771.58, 10052.34)
c7<-c(1562.95, 1926.39, 3721.81, 4053.54, 5330.07, 6693.18)
c8<-c(26.12, 6234.08, 9465.25, 9817.33, 10681.84, 12977.78, 14396.38)
c9<-c(8442.23, 8571.35, 8674.22, 10724.29, 11274.46, 11755.99)
c10<-c(708.46, 2334.95, 8018.72, 11369.85, 13432.91, 16321.18)
lista=list(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10)
#Modify the distribution as needed 
vero_pp <- function(alpha){
  alpha1=exp(alpha[1])
  alpha2=exp(alpha[2])
  s=0
  for (i in 1:l1){
    dt=lista[[i]][1]
    s1=1-plnorm(dt,alpha1,alpha2)
    #s1=exp(-(1/alpha)*dt)
    s=s+log(s1)
  }
  for (i in (l1+1):(l1+l2)){
    t=0
    d=lista[[i]]
    d1=length(d)
    for (i1 in 1:(d1-1)){
      
      #s1=(1/alpha)*exp(-(1/alpha)*(d[i1]-t))
      s1=dlnorm((d[i1]-t),alpha1,alpha2)
      s=s+log(s1)
      t=d[i1]
    }
    #s1=exp(-(1/alpha)*(d[d1]-t))
    s1=1-plnorm((d[d1]-t),alpha1,alpha2)
    s=s+log(s1)
  }
  
  for (i in (l2+l1+1):(l2+l1+m)){
    t=0
    d=lista[[i]]
    d1=length(d)
    for (i1 in 1:d1){
      #s1=(1/alpha)*exp(-(1/alpha)*(d[i1]-t))
      s1=dlnorm((d[i1]-t),alpha1,alpha2)
      s=s+log(s1)
      t=d[i1]
    }
    
  }
  
  return(-s)
  
  
  }
#Change as needed.  
#P=optim(c(log(7),log(0.45)), vero_pp)
P <- optim(c(log(7),log(0.45)),  # Initial values
  vero_pp,          # Likelihood function
  method = "BFGS",  # Optimization method
  hessian = TRUE,
  control = list(
    maxit = 50000,    # Increase the maximum number of iterations
    reltol = 1e-40,   # Set relative tolerance for convergence
    abstol = 1e-15   # Set absolute tolerance (if needed)
    
  )
)

options(digits = 6)
#hessian_value <- hessian(vero_pp, P$par)
hessian_value<-P$hessian
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