getwd()
setwd("C:/Users/LL/Desktop")

a<-read.csv('try110day32.csv',header = TRUE)
a1=0.96
a2=0.60
a3=1.45
b1=0.22
b2=0.29
b3=0.21
r1=0.26
r2=0.39
r3=0.09
C1=2.02
C2=1.49
C3=1.97
D1=1.02
D2=1.04
D3=1.55

p1<-a[,2]
p2<-a[,8]
p3<-a[,14]
n1<-a[,3]
n2<-a[,9]
n3<-a[,15]
h1<-a[,5]
h2<-a[,11]
h3<-a[,17]
q1<-a[,4]
q2<-a[,10]
q3<-a[,16]
fm=exp(a1-b1*p1+r1*log(C1*h1+D1))+exp(a2-b2*p2+r2*log(C2*h2+D2))+exp(a3-b3*p3+r3*log(C3*h3+D3))
qq1=exp(a1-b1*p1+r1*log(C1*h1+D1))/fm
qq2=exp(a2-b2*p2+r2*log(C2*h2+D2))/fm
qq3=exp(a3-b3*p3+r3*log(C3*h3+D3))/fm

sMAPEfz=sum(abs(qq1-q1)/((qq1+q1)))+sum(abs(qq2-q2)/((qq2+q2)))+sum(abs(qq3-q3)/((qq3+q3)))
sMAPE=sMAPEfz/270

RMSEfz=sum((qq1-q1)^2)+sum((qq2-q2)^2)+sum((qq3-q3)^2)
RMSE=sqrt(RMSEfz/270)

MAPEfz=sum(abs(qq1-q1))+sum(abs(qq2-q2))+sum(abs(qq3-q3))
MAPE=MAPEfz/270

pi=sum(p1*q1+p2*q2+p3*q3)
ppi=sum(p1*qq1+p2*qq2+p3*qq3)
sMAPEp=abs(pi-ppi)/(90*3)
RMSEp=sqrt((pi-ppi)^2/(90*3))