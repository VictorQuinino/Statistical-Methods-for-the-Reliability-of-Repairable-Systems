library(pracma)
clear()
#Define the time t for evaluation
t=10000
####Matrix for failure recording####
rows=5000
coluns=500 #Increase if Necessary
####################################
aprox=1000 #Upper limit of the summation of expression 1. 
#Increase if Necessary.
#Among the estimated parameters of the Lifetime Distribution
#Expoencial=1, Weibull=2, Gamma=3, lognormal=4, etc
Choice=4 #Change if Desired.Choice of distribution. 
#Adjust the parameter estimates as desired
if (Choice==1){ #Expoencial
hat_alpha=2090.13 #Mean
M=0
for ( i in 1: aprox){
  M1=pgamma(t,i*1,1/hat_alpha)
  M=M+M1
}
original_matrix<-matrix(rexp(rows*coluns,rate=1/hat_alpha),rows,coluns)
}
if (Choice==2){ #Weibull
shapeW=2.207127
scaleW=2360.036
mw=scaleW*gamma(1+1/shapeW) 
vw=(scaleW^2)*(gamma(1+2/shapeW)-(gamma(1+1/shapeW)^2)) 
M=pweibull(t,shapeW,scaleW)
for ( i in 2: aprox){
  M1=pnorm(t,i*mw,(i*vw)^0.5)
  M=M+M1
}
original_matrix<-matrix(rweibull(rows*coluns,shapeW, scaleW),rows,coluns)
}
if (Choice==3){ #Gamma
shapeG=4.368643
scaleG=0.00209013
mG=shapeG/scaleG #Média
vg=shapeG/(scaleG^2)
M=0
for ( i in 1: aprox){
  M1=pgamma(t,i*shapeG,scaleG)
  M=M+M1
}
original_matrix<-matrix(rgamma(rows*coluns,shapeG, scaleG),rows,coluns)
}
if (Choice==4){ #lognormal
  locationL=7.54192
  scaleL=0.4540074
  ml=exp(locationL+(scaleL^2)/2)
  vl=(exp(scaleL^2)-1)*exp(2*locationL+scaleL^2)
  M=plnorm(t,locationL,scaleL)
  for ( i in 2: aprox){
    M1=pnorm(t,i*ml,(i*vl)^0.5)
    M=M+M1
  }
 original_matrix<-matrix(rlnorm(rows*coluns,locationL, scaleL),rows,coluns)
}
cumulative_matrix <- original_matrix  # Initialize the cumulative matrix
for (i in 1:rows) {
  cumulative_matrix[i, ] <- cumsum(original_matrix[i, ])
}
count_less_than_t <- rowSums(cumulative_matrix <= t)
Mean_Failures=mean(count_less_than_t)
cat('Mean_Failures Simulation Monte Carlo=',Mean_Failures,"\n")
cat('Mean_Failures Expression 2=',M,"\n") 

  

