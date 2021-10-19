getwd()
setwd("C:/Users/LL/Desktop")

a<-read.csv('try2500day3.csv',header = TRUE)
a1=1.95
a2=1.97
a3=2.08
b1=0.1
b2=0.14
b3=0.10


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
fm=exp(a1-b1*p1)+exp(a2-b2*p2)+exp(a3-b3*p3)
qq1=exp(a1-b1*p1)/fm
qq2=exp(a2-b2*p2)/fm
qq3=exp(a3-b3*p3)/fm
sMAPEfz=sum(abs(qq1-q1)/((qq1+q1)))+sum(abs(qq2-q2)/((qq2+q2)))+sum(abs(qq3-q3)/((qq3+q3)))
sMAPE=sMAPEfz/73
RMSEfz=sum((qq1-q1)^2)+sum((qq2-q2)^2)+sum((qq3-q3)^2)
RMSE=sqrt(RMSEfz/73)

pi=sum(p1*q1+p2*q2+p3*q3)
ppi=sum(p1*qq1+p2*qq2+p3*qq3)
sMAPEp=abs(pi-ppi)/(73*3)
RMSEp=sqrt((pi-ppi)^2/(73*3))