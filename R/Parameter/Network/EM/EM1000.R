getwd()
setwd("C:/Users/LL/Desktop")

a<-read.csv('try2500day3.csv',header = TRUE)
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
x1<-a[,6]
x2<-a[,12]
x3<-a[,18]
a1=2
a2=2
a3=2
b1=1
b2=1
b3=1
C1=5
C2=5
C3=5
D1=1
D2=1
D3=1
r1=8
r2=8
r3=8
m1=0
m2=0
m3=0
m=0
q1=0
q2=0
q3=0
q0=0
n0=0
iter = 1
da1=0
da2=0
da3=0
db1=0
db2=0
db3=0
dC1=0
dC2=0
dC3=0
dD1=1
dD2=1
dD3=1
dr1=0
dr2=0
dr3=0
nonstop=TRUE
while(nonstop){
  #E
  n<-n1+n2+n3
  for (t in 1:73) {
    m1[t]<-exp(a1-b1*p1[t]+r1*log(C1*h1[t]+D1))
    m2[t]<-exp(a2-b2*p2[t]+r2*log(C2*h2[t]+D2))
    m3[t]<-exp(a3-b3*p3[t]+r3*log(C3*h3[t]+D3))
    m[t]<-1+m1[t]+m2[t]+m3[t]
    q1[t]<-m1[t]/m[t]
    q2[t]<-m2[t]/m[t]
    q3[t]<-m3[t]/m[t]
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
  db1<-sum((n+n0)*p1*q1-p1*n1)
  db2<-sum((n+n0)*p2*q2-p2*n2)
  db3<-sum((n+n0)*p3*q3-p3*n3)
  dr1<-sum((n1*log(C1*h1+D1)-(n+n0)*q1*log(C1*h1+D1)))
  dr2<-sum((n2*log(C2*h2+D2)-(n+n0)*q2*log(C2*h2+D2)))
  dr3<-sum((n3*log(C3*h3+D3)-(n+n0)*q3*log(C3*h3+D3)))
  dC1<-sum(n1*r1*h1/(C1*h1+D1)-(n+n0)*q1*r1*h1/(C1*h1+D1))
  dC2<-sum(n2*r2*h2/(C2*h2+D2)-(n+n0)*q2*r2*h2/(C2*h2+D2))
  dC3<-sum(n3*r3*h3/(C3*h3+D3)-(n+n0)*q3*r3*h3/(C3*h3+D3))
  dD1<-sum(n1*r1/(C1*h1+D1)-(n+n0)*r1*q1/(C1*h1+D1))
  dD2<-sum(n2*r2/(C2*h2+D2)-(n+n0)*r2*q2/(C2*h2+D2))
  dD3<-sum(n3*r3/(C3*h3+D3)-(n+n0)*r3*q3/(C3*h3+D3))
  
  olda1<-a1
  olda2<-a2
  olda3<-a3
  oldb1<-b1
  oldb2<-b2
  oldb3<-b3
  oldr1<-r1
  oldr2<-r2
  oldr3<-r3
  oldC1<-C1
  oldC2<-C2
  oldC3<-C3
  oldD1<-D1
  oldD2<-D2
  oldD3<-D3
  oldLL1<-0
  oldLL2<-0
  oldm1<-exp(olda1-oldb1*p1+oldr1*log(oldC1*h1+oldD1))
  oldm2<-exp(olda2-oldb2*p2+oldr2*log(oldC2*h2+oldD2))
  oldm3<-exp(olda3-oldb3*p3+oldr3*log(oldC3*h3+oldD3))
  oldm<-1+oldm1+oldm2+oldm3
  for (t in 1:73) {
    oldLL1[t]<-n1[t]*(olda1-oldb1*p1[t]+oldr1*log(oldC1*h1[t]+oldD1))+n2[t]*(olda2-oldb2*p2[t]+oldr2*log(oldC2*h2[t]+oldD2))+n3[t]*(olda3-oldb3*p3[t]+oldr3*log(oldC3*h3[t]+oldD3))
    oldLL2[t]<-n[t]*log(oldm[t])
  }
  
  oldLL<-sum(oldLL1-oldLL2)
  
  s<-0.00001
  r<-0.5
  
  a1<-olda1+s*(da1)
  a2<-olda2+s*(da2)
  a3<-olda3+s*(da3)
  b1<-oldb1+s*(db1)
  b2<-oldb2+s*(db2)
  b3<-oldb3+s*(db3)
  r1<-oldr1+s*(dr1)
  r2<-oldr2+s*(dr2)
  r3<-oldr3+s*(dr3)
  C1<-oldC1+s*(dC1)
  C2<-oldC2+s*(dC2)
  C3<-oldC3+s*(dC3)
  D1<-oldD1+s*(dD1)
  D2<-oldD2+s*(dD2)
  D3<-oldD3+s*(dD3)
  
  LL1<-0
  LL2<-0
  for (t in 1:73) {
    LL1[t]<-n1[t]*(a1-b1*p1[t]+r1*log(C1*h1[t]+D1))+n2[t]*(a2-b2*p2[t]+r2*log(C2*h2[t]+D2))+n3[t]*(a3-b3*p3[t]+r3*log(C3*h3[t]+D3))
    m1[t]<-exp(a1-b1*p1[t]+r1*log(C1*h1[t]+D1))
    m2[t]<-exp(a2-b2*p2[t]+r2*log(C2*h2[t]+D2))
    m3[t]<-exp(a3-b3*p3[t]+r3*log(C3*h3[t]+D3))
    m[t]<-1+m1[t]+m2[t]+m3[t]
    LL2[t]<-n[t]*log(m[t])
  }
  LL<-sum(LL1[t]-LL2[t])
  ii=0
  ddd<-((da1)^2+(da2)^2+(da3)^2+(db1)^2+(db2)^2+(db3)^2+(dr1)^2+(dr2)^2+(dr3)^2+(dC1)^2+(dC2)^2+(dC3)^2+(dD2)^2+(dD1)^2+(dD3)^2)
  while (LL > oldLL + s*r*(ddd)^(1/2)){
    if(s>0.000001){
      ii=ii+1
      s= s*r
      a1<-olda1+s*(da1)
      a2<-olda2+s*(da2)
      a3<-olda3+s*(da3)
      b1<-oldb1+s*(db1)
      b2<-oldb2+s*(db2)
      b3<-oldb3+s*(db3)
      r1<-oldr1+s*(dr1)
      r2<-oldr2+s*(dr2)
      r3<-oldr3+s*(dr3)
      C1<-oldC1+s*(dC1)
      C2<-oldC2+s*(dC2)
      C3<-oldC3+s*(dC3)
      D1<-oldD1+s*(dD1)
      D2<-oldD2+s*(dD2)
      D3<-oldD3+s*(dD3)
      for (t in 1:73) {
        LL1[t]=n1[t]*(a1-b1*p1[t]+r1*log(C1*h1[t]+D1))+n2[t]*(a2-b2*p2[t]+r2*log(C2*h2[t]+D2))+n3[t]*(a3-b3*p3[t]+r3*log(C3*h3[t]+D3))
        m1[t]=exp(a1-b1*p1[t]+r1*log(C1*h1[t]+D1))
        m2[t]=exp(a2-b2*p2[t]+r2*log(C2*h2[t]+D2))
        m3[t]=exp(a3-b3*p3[t]+r3*log(C3*h3[t]+D3))
        m[t]=1+m1[t]+m2[t]+m3[t]
        LL2[t]=n[t]*log(m[t])
      }
      LL=sum(LL1[t]-LL2[t])
    }else{
      break
    }
    
  }
  
  iter = iter + 1
  nonstop <- ((ddd)^(1/2))>100
}
AAA<-c(a1,a2,a3,0,b1,b2,b3,0,r1,r2,r3,0,C1,C2,C3,0,D1,D2,D3)
