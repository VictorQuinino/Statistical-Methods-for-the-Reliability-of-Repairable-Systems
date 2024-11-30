library(pracma)
clear()
l1=2
l2=4
m=4
T=l2+m
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
lista=list(c3,c4,c5,c6,c7,c8,c9,c10)

Resul=matrix(0,T,3)
for (i in 1:T){
  Resul[i,1]=(sum(lista[[i]])-lista[[i]][length(lista[[i]])])
  Resul[i,2]=(length(lista[[i]])-1)*max(lista[[i]])/2
  Resul[i,3]=(max(lista[[i]])^2)*(length(lista[[i]])-1)*(1/12)
  
}
F=(sum(Resul[,1])-sum(Resul[,2]))/((sum(Resul[,3]))^0.5)
pvalue_Laplace=min(c(pnorm(F)*2, (1-(pnorm(F)))*2))
k=1
Res=matrix(0,T,1)
Res1=matrix(0,T,1)
for (i in 1:T){
  T1=(length(lista[[i]])-1)
  Res1[i]=T1
  for (j in 1:T1){
  Res[k]=2*log(max(lista[[i]])/lista[[i]][j])
  k=k+1
}
}
F1=sum(Res)
q=sum(Res1)
pvalue_MH189=min(c(pchisq(F1,2*q)*2, (1-(pchisq(F1,2*q)))*2))

cat('Laplace’s Test Pooled =',F,"\n")
cat('Laplace’s Test Pooled p-value=', pvalue_Laplace, "\n")
cat('MIL-Hdbk-189 Test Pooled =',F1,"\n")
cat('MIL-Hdbk-189 Test Pooled p-value=', pvalue_MH189, "\n")



