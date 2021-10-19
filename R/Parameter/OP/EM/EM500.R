getwd()
setwd("C:/Users/LL/Desktop")

a<-read.csv('try500day3.csv',header = TRUE)
#mydata<-read.table(header=T,file="C:/Users/LL/Desktop/try2500day2.csv",sep=",")
p1<-a[,2]
p2<-a[,8]
p3<-a[,14]
n1<-a[,3]
n2<-a[,9]
n3<-a[,15]
h1<-a[,5]
h2<-a[,11]
h3<-a[,17]
p4<-a[,20]
n4<-a[,21]
h4<-a[,23]

a1=1
a2=1
a3=1
a4=1
b1=1
b2=1
b3=1
b4=1

m1=0
m2=0
m3=0
m=0
q1=0
q2=0
q3=0
q4=0
m4=0
q0=0
n0=0
iter = 1
da1=0
da2=0
da3=0
da4=0
db1=0
db2=0
db3=0
db4=0

nonstop=TRUE
while(nonstop){
  #E
  n<-n1+n2+n3+n4
  for (t in 1:88) {
    m1[t]<-exp(a1-b1*p1[t])
    m2[t]<-exp(a2-b2*p2[t])
    m3[t]<-exp(a3-b3*p3[t])
    m4[t]<-exp(a4-b4*p4[t])
    m[t]<-1+m1[t]+m2[t]+m3[t]+m4[t]
    q1[t]<-m1[t]/m[t]
    q2[t]<-m2[t]/m[t]
    q3[t]<-m3[t]/m[t]
    q4[t]<-m4[t]/m[t]
    q0[t]<-1/m[t]
    n0[t]<-q0[t]*n[t]/(1-q0[t])
  }
  #M
  #L1=expression((n1*(A1-B1*p1+R1*log(CC1*h1+DD1))+n2*(A2-B2*p2+R2*log(CC2*h2+DD2))+n3*(A3-B3*p3+R3*log(CC3*h3+DD3))))
  #L2=expression((1+exp(A3-B3*p3+R3*log(CC3*h3+DD3))+exp(A2-B2*p2+R2*log(CC2*h2+DD2))+exp(A1-B1*p1+R1*log(CC1*h1+DD1))))
  #L=expression(L1-L2)
  #d<- deriv((L),c("A1","A2","A3","B1","B2","B3","R1","R2","R3","CC1","CC2","CC3","DD1","DD2","DD3"),function.arg = TRUE)
  #f=expression(sum(n1[t]*(a1-b1*p1[t]+r1*log(C1*h1[t]+D1))+n2[t]*(a2-b2*p2[t]+r2*log(C2*h2[t]+D2))+n3[t]*(a3-b3*p3[t]+r3*log(C3*h3[t]+D3))-n[t]*log(1+exp(a1-b1*p1[t]+r1*log(C1*h1[t]+D1))+exp(a2-b2*p2[t]+r2*log(C2*h2[t]+D2))+exp(a3-b3*p3[t]+r3*log(C3*h3[t]+D3)))))
  #d<- deriv(L,c("a1","a2"),function.arg = TRUE)
  #dd<-d(a1,a2,a3,b1,b2,b3,r1,r2,r3,CC1,CC2,CC3,DD1,DD2,DD3)
  #ddd<-attr(,"gradient")
  
  
  da1<-sum(n1-(n+n0)*q1)
  da2<-sum(n2-(n+n0)*q2)
  da3<-sum(n3-(n+n0)*q3)
  da4<-sum(n4-(n+n0)*q4)
  db1<-sum((n+n0)*p1*q1-p1*n1)
  db2<-sum((n+n0)*p2*q2-p2*n2)
  db3<-sum((n+n0)*p3*q3-p3*n3)
  db4<-sum((n+n0)*p4*q4-p4*n4)
 
  olda1<-a1
  olda2<-a2
  olda3<-a3
  olda4<-a4
  oldb1<-b1
  oldb2<-b2
  oldb3<-b3
  oldb4<-b4
  
  oldLL1<-0
  oldLL2<-0
  oldm1<-exp(olda1-oldb1*p1)
  oldm2<-exp(olda2-oldb2*p2)
  oldm3<-exp(olda3-oldb3*p3)
  oldm4<-exp(olda4-oldb4*p4)
  oldm<-1+oldm1+oldm2+oldm3+oldm4
  for (t in 1:88) {
    oldLL1[t]<-n4[t]*(olda4-oldb4*p4[t])+n1[t]*(olda1-oldb1*p1[t])+n2[t]*(olda2-oldb2*p2[t])+n3[t]*(olda3-oldb3*p3[t])
    oldLL2[t]<-n[t]*log(oldm[t])
  }
  
  oldLL<-sum(oldLL1-oldLL2)
  
  s<-0.00001
  r<-0.5
  
  a1<-olda1+s*(da1)
  a2<-olda2+s*(da2)
  a3<-olda3+s*(da3)
  a4<-olda4+s*(da4)
  b1<-oldb1+s*(db1)
  b2<-oldb2+s*(db2)
  b3<-oldb3+s*(db3)
  b4<-oldb4+s*(db4)
 
  LL1<-0
  LL2<-0
  for (t in 1:88) {
    LL1[t]<-n1[t]*(a1-b1*p1[t])+n2[t]*(a2-b2*p2[t])+n3[t]*(a3-b3*p3[t])+n4[t]*(a4-b4*p4[t])
    m1[t]<-exp(a1-b1*p1[t])
    m2[t]<-exp(a2-b2*p2[t])
    m3[t]<-exp(a3-b3*p3[t])
    m4[t]<-exp(a4-b4*p4[t])
    m[t]<-1+m1[t]+m2[t]+m3[t]+m4[t]
    LL2[t]<-n[t]*log(m[t])
  }
  LL<-sum(LL1[t]-LL2[t])
  ii=0
  ddd<-((da1)^2+(da2)^2+(da3)^2+(da4)^2+(db4)^2+(db1)^2+(db2)^2+(db3)^2^2)
  while (LL > oldLL + s*r*(ddd)^(1/2)){
    if(s>0.000001){
      ii=ii+1
      s= s*r
      a1<-olda1+s*(da1)
      a2<-olda2+s*(da2)
      a3<-olda3+s*(da3)
      a4<-olda4+s*(da4)
      b1<-oldb1+s*(db1)
      b2<-oldb2+s*(db2)
      b3<-oldb3+s*(db3)
      b4<-oldb4+s*(db4)
      
      for (t in 1:88) {
        LL1[t]=n1[t]*(a1-b1*p1[t])+n2[t]*(a2-b2*p2[t])+n3[t]*(a3-b3*p3[t])+n4[t]*(a4-b4*p4[t])
        m1[t]=exp(a1-b1*p1[t])
        m2[t]=exp(a2-b2*p2[t])
        m3[t]=exp(a3-b3*p3[t])
        m4[t]<-exp(a4-b4*p4[t])
        m[t]<-1+m1[t]+m2[t]+m3[t]+m4[t]
        LL2[t]=n[t]*log(m[t])
      }
      LL<-sum(LL1[t]-LL2[t])
    }else{
      break
    }
    
  }
  
  iter = iter + 1
  nonstop <- ((ddd)^(1/2))>200
}

