library(pracma)
library(numDeriv)
clear()
l1=2
l2=7
m=17
#Data #Change as needed. 
c1<-c(650)
c2<-c(600)
c3<-c(348, 514, 600)
c4<-c(375, 379, 489, 604, 650)
c5<-c(276, 479, 550)
c6<-c(171, 479, 500)
c7<-c(496, 502, 550)
c8<-c(312, 519, 550)
c9<-c(203, 503, 505, 621, 628, 650)
c10<-c(187, 502, 634)
c11<-c(274, 296, 497, 501, 511)
c12<-c(182, 485, 510)
c13<-c(510, 544)
c14<-c(484, 490, 590, 609)
c15<-c(276, 486, 597)
c16<-c(491, 513, 532)
c17<-c(269, 276, 505)
c18<-c(499, 520)
c19<-c(275, 491, 604, 631)
c20<-c(353, 533)
c21<-c(468, 481, 612)
c22<-c(392, 494, 520)
c23<-c(281, 383, 448, 506)
c24<-c(480, 492, 504)
c25<-c(409, 520)
c26<-c(290, 490, 505, 585)
lista=list(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14,c15,c16,c17,c18,c19,
           c20, c21, c22, c23, c24, c25, c26)
#Modify the distribution as needed 
vero_pp <- function(alpha){
  alpha1=exp(alpha[1])
  alpha2=exp(alpha[2])
  s=0
  for (i in 1:l1){
    dt=lista[[i]][1]
    s1=1-pweibull(dt,alpha1,alpha2)
    s=s+log(s1)
  }
  for (i in (l1+1):(l1+l2)){
    t=0
    d=lista[[i]]
    d1=length(d)
    for (i1 in 1:(d1-1)){
      s1=dweibull((d[i1]),alpha1,alpha2)/(1-pweibull(t,alpha1,alpha2))
      s=s+log(s1)
      t=d[i1]
    }
    s1=(1-pweibull((d[d1]),alpha1,alpha2))/(1-pweibull(t,alpha1,alpha2))
    s=s+log(s1)
  }
  
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
P <- optim(c(log(4),log(500)),  # Initial values
  vero_pp,          # Likelihood function
  method = "BFGS",  # Optimization method
  hessian = TRUE,
  control = list(
    maxit = 50000,    # Increase the maximum number of iterations
    reltol = 1e-40,   # Set relative tolerance for convergence
    abstol = 1e-25   # Set absolute tolerance (if needed)
    
    
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