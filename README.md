<h1 align="center">Myopic Policy</h1>
<!-- TABLE OF CONTENTS -->
<details open="open">
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#background">Background</a>
    </li>
    <li>
      <a href="#tools">Tools</a>
      <ul>
        <li><a href="#sas">SAS</a></li>
        <li><a href="#r">R</a></li>
        <li><a href="#matlab">MATLAB</a></li>
      </ul>
    </li>
    <li><a href="#usage">Usage</a></li>
    <ul>
   <li><a href="#sas-files">SAS-Files</a></li>
            <li><a href="#r-files">R-Files</a></li>
        <li><a href="#matlab-files">MATLAB-Files</a></li>
    </ul>
    </li>
    <li><a href="#contact">Contact</a></li>
  </ol>
</details>

<!-- Background -->
## Background
There are three purposes of this project, the first is to test influence factors of consumer utility, i.e. only price or price combined with network effect. Next, we estimating the parameters of the factor of consumer utility, and use the parameters to predict consumers' decisions and retailers' revenue. The third is to make pricing decision, and give the gap between our pricing policy and optimal policy.

<!-- Tools -->
## Tools
The application of this project requires three tools, SAS, R and MATLAB software.
### SAS
First, we use SAS software to compare the model expressiveness and prediction accuracy of the two models without network effect and with network effect, and test whether the consumer utility is affected by network effect. Moreover, by comparing the utility models of network effects in different functional forms, the influence forms of network effects can be judged. Specifically, it can be divided into linear form and logarithmic form.
### R
Next, we use R studio software to estimate the parameter of different models. Specifically, we write EM algorithm to estimate parameter and use boostrap method to do the t-test. Then we use R studio software to predict sales and revenue according to the parameters above.
### MATLAB
The parameters of different impact factors are estimated by the above analysis conclusions, and pricing decisions and revenue under different pricing policy are calculated by MATLAB software. Specifically, it includes optimal pricing policy, myopic pricing policy, and standard MNL model pricing policy without considering network effects. In the end, we use MATLAB software to calculate the gap between our pricing policy and optimal policy.

<!-- USAGE EXAMPLES -->
## Usage
<!-- SAS Files -->
### SAS-Files
There are three folders in the SAS folder, which correspond to SAS codes of different kinds of products. The following takes 110g menlon seeds as an example to explain the use of relevant codes.<br>

In the 110g folder, there are three SAS code files, among which "try110nop.sas" means that in the consumer utility function, only the influence of product price is considered; "try110xx.sas" means that in the consumer utility function, the influence of linear form of network effect is considered. "try110.sas" means that the network effect in logarithmic form is considered in the consumer utility function. Take the try110.sas code file as an example.<br>

First, we import the "TRY110" data file and explain the contents of the data file. In this case, the data file is an Excel file, and the fields include: classification variable melon_seeds, where "1" represents caramal seeds, "2" represents red date seeds, "3" represents original taste seeds, and "4" means nonpurchase people. "price" is the price of the product, "x" is the sales volume in the last month of each product. "ln" is the network effect, where ln=log (Cx+D), the values of C and D are related to the product.<br>
```
proc contents data = WORK.TRY110;
	run;
  
proc format;
value melon_seeds_l 
  1="caramel"
  2="red date" 
  3="origin"
  4="nonpurchase";
run;
```
After that, we make descriptive statistics for the relevant data in the data file, and the code is as follows：<br>
```
proc freq data = WORK.TRY110;
	format melon_seeds melon_seeds_l.;
	table melon_seeds;
run;

proc means data = WORK.TRY110;
	format melon_seeds melon_seedsl.;
	var price ln= WORK.TRY110;
	run;
```
Finally, logistic regression in SAS is used to test the explanatory power and prediction accuracy of the model. In this example, we get the results of the model under the dual influence of price and logarithmic network effect. The following is a code example:<br>
```
proc logistic data = WORK.TRY110;
	model melon_seeds = price ln/ link = glogit;
run;
```
<!-- R Files -->
### R-Files
Under the R folder we created two folders, the "Parameter" folder is to estimate parameter of different models, and the "Test" folder is to predict the sales and revenue according to the parameters.
<!-- parameter -->
#### Parameter Files
Under this folder, we created two folders, the "Network" folder means that the codes in this folder consider the network effect, and the "OP" folder means that the codes only consider the effect of price. And folders are symmetric in "Network" and "OP" folders, so we adopt the "Network" folder as an example to explain our work.
<!-- EM -->
#### 1. EM Files
First, we import the "EM110" code file and explain the contents of the code file.  In this case, the data file is an Excel file, and the fields include: price of each product on sales day (p1, p2, p3), sales column of each product on sales day (n1, n2, n3) and history sales in last month of each product  (h1, h2, h3). Then we build the initial value of the parameters.<br>
```
getwd()
setwd("C:/Users/LL/Desktop")

a<-read.csv('try110day32.csv',header = TRUE)
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
a1=1
a2=1
a3=1
b1=4
b2=4
b3=4
C1=2
C2=2
C3=2
D1=1
D2=1
D3=1
r1=3
r2=3
r3=3
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
```
After that, we use EM algorithm to estimate the parameters, first, in E step, we caculate the purchasing probability according to initial value.
```
#E
  n<-n1+n2+n3
  for (t in 1:87) {
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
```
Then in M step, we caculate derivative of each parameter to get the log value using maximum likelihood method.
```
#M
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
  for (t in 1:87) {
    oldLL1[t]<-n1[t]*(olda1-oldb1*p1[t]+oldr1*log(oldC1*h1[t]+oldD1))+n2[t]*(olda2-oldb2*p2[t]+oldr2*log(oldC2*h2[t]+oldD2))+n3[t]*(olda3-oldb3*p3[t]+oldr3*log(oldC3*h3[t]+oldD3))
    oldLL2[t]<-n[t]*log(oldm[t])
  }
  
  oldLL<-sum(oldLL1-oldLL2)
  ```
 According to the derivate of each parameter, we can get the new value of each parameter and log value.
 ```
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
  for (t in 1:87) {
    LL1[t]<-n1[t]*(a1-b1*p1[t]+r1*log(C1*h1[t]+D1))+n2[t]*(a2-b2*p2[t]+r2*log(C2*h2[t]+D2))+n3[t]*(a3-b3*p3[t]+r3*log(C3*h3[t]+D3))
    m1[t]<-exp(a1-b1*p1[t]+r1*log(C1*h1[t]+D1))
    m2[t]<-exp(a2-b2*p2[t]+r2*log(C2*h2[t]+D2))
    m3[t]<-exp(a3-b3*p3[t]+r3*log(C3*h3[t]+D3))
    m[t]<-1+m1[t]+m2[t]+m3[t]
    LL2[t]<-n[t]*log(m[t])
  }
  LL<-sum(LL1[t]-LL2[t])
 ```
 Finally, we compare new log value and old log value, then use backtracking line search method to decide when we stop iteration.
 ```
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
      for (t in 1:87) {
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
 ```
<!-- BS -->
#### 2. BS Files
In this files, we use boostrap method to do t-test of conclusions of estimating parameters. Specifically, we sample some data from the Excel file randomly and repeat EM algorithm by the samples.
```
for (ttt in 1:100) {
  bs<-c(1:87)
  uu<-sample(bs,72)
  EM Algorithm
  A1[ttt]<-a1
  A2[ttt]<-a2
  A3[ttt]<-a3
  B1[ttt]<-b1
  B2[ttt]<-b2
  B3[ttt]<-b3
  R1[ttt]<-r1
  R2[ttt]<-r2
  R3[ttt]<-r3
  CC1[ttt]<-C1
  CC2[ttt]<-C2
  CC3[ttt]<-C3
  DD1[ttt]<-D1
  DD2[ttt]<-D2
  DD3[ttt]<-D3
}
```
<!-- test -->
#### Test Files
Under this folder, we created two folders, the "Network" folder means that the codes in this folder consider the network effect, and the "OP" folder means that the codes only consider the effect of price. And folders are symmetric in "Network" and "OP" folders, so we adopt the "Network" folder as an example to explain our work.<br>

First, we import the "test110" code file and explain the contents of the code file. In this case, we first input the parameters caculated above.
```
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
```
Then we caculate true value of sales and revenue in actual data.
```
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
```
Finally, we caculate SMAPE and RMSE value.
```
sMAPEfz=sum(abs(qq1-q1)/((qq1+q1)))+sum(abs(qq2-q2)/((qq2+q2)))+sum(abs(qq3-q3)/((qq3+q3)))
sMAPE=sMAPEfz/270

RMSEfz=sum((qq1-q1)^2)+sum((qq2-q2)^2)+sum((qq3-q3)^2)
RMSE=sqrt(RMSEfz/270)

pi=sum(p1*q1+p2*q2+p3*q3)
ppi=sum(p1*qq1+p2*qq2+p3*qq3)
sMAPEp=abs(pi-ppi)/(90*3)
RMSEp=sqrt((pi-ppi)^2/(90*3))
```
<!-- MATLAB Files -->
### MATLAB-Files
Under the MATLAB folder we created two folders, "solution" means that we use different pricing policy to caculate product price and revenue. "gap" means that we caculate the gap between our pricing policy and optimal policy.
<!--  Solution Files -->
#### Solution Files
Under the solution folder we created three folders based on different pricing policy, The "optimal" file, "myopic" file,  and "standard" file represent the optimal pricing policy, the myopic pricing policy, and the standard MNL model pricing strategy without network effects, respectively. We will explain how to use the code one by one.
<!-- Optimal File -->
#### 1. Optimal File
In the optimal file, we divide it by product category and sales season. Taking the "optimal110.m" code file as an example, we first give initial value and bound to solve the problem.
```
function m = optimal110(x)
x0=linspace(0.1,0.1,900);
lb=zeros(900,1);
ub1=ones(450,1);
ub2=2^2*ones(450,1);
ub=[ub1;ub2];
options=optimset('Maxfuneval',10000000); 
[y,ff]= fmincon(@(x) fminx(x),x0,[],[],[],[],lb,ub,@(x) fcontr(x),options);
end
```
After that, we caculate the revenue function in one sales season. The code is as follows:

```
function ffff =fminx(x)
a1=0.96;a2=0.60;a3=1.45;b1=0.22;b2=0.29;b3=0.21;r1=0.26;r2=0.39;r3=0.09;C1=2.02;C2=1.49;C3=1.97;D1=1.02;D2=1.04;D3=1.55;
D=2;f=0;P1=[];P2=[];P3=[];
PI=[];
for t=1:3:448
f=(...
    f+(1/b1)*D*x(t)*(a1+r1*log(C1*D*x(t+450)+D1)-log(x(t))+log(1-x(t)-x(t+1)-x(t+2)))...
    +(1/b2)*D*x(t+1)*(a2+r2*log(C2*D*x(t+451)+D2)-log(x(t+1))+log(1-x(t)-x(t+1)-x(t+2)))...
    +(1/b3)*D*x(t+2)*(a3+r3*log(C3*D*x(t+452)+D3)-log(x(t+2))+log(1-x(t)-x(t+1)-x(t+2)))...
    )
p1=(1/b1)*(a1+r1*log(C1*D*x(t+450)+D1)-log(x(t))+log(1-x(t)-x(t+1)-x(t+2)));
p2=(1/b2)*(a2+r2*log(C2*D*x(t+451)+D2)-log(x(t+1))+log(1-x(t)-x(t+1)-x(t+2)));
p3=(1/b3)*(a3+r3*log(C3*D*x(t+452)+D3)-log(x(t+2))+log(1-x(t)-x(t+1)-x(t+2)));
P1=[P1 p1];
P2=[P2 p2];
P3=[P3 p3];
end
ffff=-f
P1
P2
P3
end
```
Finally, we write the constraints of our problem, i.e. the history sales constraints and probability constraints.
```
function [cc,w]=fcontr(x)
for j=1:450
if j>90 & mod(j-1,3)==0
ii=0;
for t=j-90:3:j-1
ii=ii+x(t);
end
w(j)=ii-x(j+450);
elseif j<=90 & mod(j-1,3)==0
ii=0;
for t=1:3:j
ii=ii+x(t);
end
w(j)=ii-x(j+450)-x(j);
elseif j>91 & mod(j-2,3)==0
ii=0;
for t=j-90:3:j-1
i=ii+x(t);
end
w(j)=ii-x(j+450);
elseif j<=92 & mod(j-2,3)==0
ii=0;
for t=2:3:j-1
ii=ii+x(t);
end
w(j)=ii-x(j+450);
elseif j>93 & mod(j-3,3)==0
ii=0;
for t=j-90:3:j-1
ii=ii+x(t);
end
w(j)=ii-x(j+450);
elseif j<=93 & mod(j-3,3)==0
ii=0;
for t=3:3:j-1
ii=ii+x(t);
end
w(j)=ii-x(j+450);
end
end
for t=1:3:448
cc(t)=x(t)+x(t+1)+x(t+2)-1;
end
end
```
<!-- Myopic File -->
#### 2. Myopic File
In the myopic file, we divide the products according to the product category and sales season. Taking the code file "myopic110.m" as an example, we first input the correlation coefficient required by the model.
```
clear
a1=0.96;a2=0.60;a3=1.45;b1=0.22;b2=0.29;b3=0.21;r1=0.26;r2=0.39;r3=0.09;C1=2.02;C2=1.49;C3=1.97;D1=1.02;D2=1.04;D3=1.55;
w1=0;w2=0;w3=0;
t=1;D=2;
Q1=zeros(1,30);Q2=zeros(1,30);Q3=zeros(1,30);
X1=[];X2=[];X3=[];
P1=[];P2=[];P3=[];
PI=[];
```
After that, the market share is established as the decision variable and the single period profit maximization is taken as the goal. The code is as follows:
```
while t<=150
    f=@(x)(-...
       ((1/b1)*x(1)*(a1+r1*log(C1*D*w1+D1)-log(x(1))+log(1-x(1)-x(2)-x(3)))...
       +(1/b2)*x(2)*(a2+r2*log(C2*D*w2+D2)-log(x(2))+log(1-x(1)-x(2)-x(3)))...
       +(1/b3)*x(3)*(a3+r3*log(C3*D*w3+D3)-log(x(3))+log(1-x(1)-x(2)-x(3)))...
       ));
   x0=[0.01;0.01;0.01];
   lb=[0;0;0];
   ub=[1;1;1];
   [xx,fval] = fmincon(f,x0,[],[],[],[],lb,ub);
   Q1=[Q1 xx(1)];
   Q1(1)=[];
   w1=sum(Q1);
   Q2=[Q2 xx(2)];
   Q2(1)=[];
   w2=sum(Q2);
   Q3=[Q3 xx(3)];
   Q3(1)=[];
   w3=sum(Q3);
   X1=[X1 xx(1)];
   X2=[X2 xx(2)];
   X3=[X3 xx(3)];
```
Finally, price and total profit are solved according to the one-to-one correspondence between market share and price:
```
   p1=(1/b1)*(a1-log(xx(1))+log(1-xx(1)-xx(2)-xx(3))+r1*log(C1*D*w1+D1));
   p2=(1/b2)*(a2-log(xx(2))+log(1-xx(1)-xx(2)-xx(3))+r2*log(C2*D*w2+D2));
   p3=(1/b3)*(a3-log(xx(3))+log(1-xx(1)-xx(2)-xx(3))+r3*log(C3*D*w3+D3));
   P1=[P1 p1];
   P2=[P2 p2];
   P3=[P3 p3];
   pi=p1*D*xx(1)+p2*D*xx(2)+p3*D*xx(3);
   PI=[PI pi];
   t=t+1;
end
```
<!-- Standard File -->
#### 3. Standard File
In the standard file, we divided the products according to product categories and sales season. Taking the code file "standard110.m" as an example, we first input the correlation coefficient required by the model.
```
clear
a1=1.01;a2=1.29;a3=0.70;b1=0.23;b2=0.31;b3=0.28;
t=1;D=2;
X1=[];X2=[];X3=[];
P1=[];P2=[];P3=[];
PI=[];
```
After that, the market share is established as the decision variable and the single-period profit maximization is taken as the goal. The code is as follows:
```
while t<=1
    f=@(x)(-...
       ((1/b1)*x(1)*(a1-log(x(1))+log(1-x(1)-x(2)-x(3)))...
       +(1/b2)*x(2)*(a2-log(x(2))+log(1-x(1)-x(2)-x(3)))...
       +(1/b3)*x(3)*(a3-log(x(3))+log(1-x(1)-x(2)-x(3)))...
       ));
   x0=[0.01;0.01;0.01];
   lb=[0;0;0];
   ub=[1;1;1];
   [xx,fval] = fmincon(f,x0,[],[],[],[],lb,ub);
```
Finally, price and total profit are solved according to the one-to-one correspondence between market share and price:
```
   p1=(1/b1)*(a1-log(xx(1))+log(1-xx(1)-xx(2)-xx(3)));
   p2=(1/b2)*(a2-log(xx(2))+log(1-xx(1)-xx(2)-xx(3)));
   p3=(1/b3)*(a3-log(xx(3))+log(1-xx(1)-xx(2)-xx(3)));
   P1=[P1 p1];
   P2=[P2 p2];
   P3=[P3 p3];
   pi=p1*D*xx(1)+p2*D*xx(2)+p3*D*xx(3);
   PI=[PI pi];
   t=t+1;
end
```
<!-- GAP File -->
### Gap Files
This folder caculate the upper bound of optimal policy revenue and lower bound of myopic policy revenue. Taking the code file "gap110.m" and "gap1102"as example, we first explain "gap110.m".<br>
"gap110.m" caculate the lower bound of myopic policy revenue, first we input the parameters.
```
clear
a1=0.96;a2=0.60;a3=1.45;b1=0.22;b2=0.29;b3=0.21;r1=0.26;r2=0.39;r3=0.09;C1=2.02;C2=1.49;C3=1.97;D1=1.02;D2=1.04;D3=1.55;
theta=0.0001;
T=210;
D=17;
L=30;
```
Then, we can get the lower bound.
```
f=@(x)(-((x(1)/b1)*(a1+r1*log(D1)-log(x(1))+log(1-x(1)-x(2)-x(3)))...
    +(x(2)/b2)*(a2+r2*log(D2)-log(x(2))+log(1-x(1)-x(2)-x(3)))...
    +(x(3)/b3)*(a3+r3*log(D3)-log(x(3))+log(1-x(1)-x(2)-x(3)))...
    ));

x0=[0.01;0.01;0.01];
lb=[0;0;0];
ub=[1;1;1];
[xx1,fval1] = fmincon(f,x0,[],[],[],[],lb,ub);
```
According to the lower bound, we get first gap of myopic policy.
```
gap1=1+(theta/((1-theta)*(1-theta^T)))*(1/fval1)*D^2*(r1*C1/(b1*D1)+r2*C2/(b2*D2)+r3*C3/(b3*D3))
```
Next, we explain "gap1102.m", first we input the parameters.
```
clear
a1=0.96;a2=0.60;a3=1.45;b1=0.22;b2=0.29;b3=0.21;r1=0.26;r2=0.39;r3=0.09;C1=2.02;C2=1.49;C3=1.97;D1=1.02;D2=1.04;D3=1.55;
theta=0.0001;
T=210;
D=17;
L=30;
```
Then, we caculate the upper bound.
```
f=@(x)(-((x(1)/b1)*(a1+r1*log(C1*((x(1)*r1/b1)*(L+D1/(D*C1)+D2/(D*C2)+D3/(D*C3))/(x(1)*r1/b1+x(2)*r2/b2+x(3)*r3/b3)-(D1/(C1*D)))+D1)-log(x(1))+log(1-x(1)-x(2)-x(3)))...
    +(x(2)/b2)*(a2+r2*log(C2*((x(2)*r2/b2)*(L+D1/(D*C1)+D2/(D*C2)+D3/(D*C3))/(x(1)*r1/b1+x(2)*r2/b2+x(3)*r3/b3)-(D2/(C2*D)))+D2)-log(x(2))+log(1-x(1)-x(2)-x(3)))...
    +(x(3)/b3)*(a3+r3*log(C3*((x(3)*r3/b3)*(L+D1/(D*C1)+D2/(D*C2)+D3/(D*C3))/(x(1)*r1/b1+x(2)*r2/b2+x(3)*r3/b3)-(D3/(C3*D)))+D3)-log(x(3))+log(1-x(1)-x(2)-x(3)))...
    ));

x0=[0.01;0.01;0.01];
lb=[0;0;0];
ub=[1;1;1];
[xx1,fval2] = fmincon(f,x0,[],[],[],[],lb,ub);
```
So we can get the second bound by the upper bound and lower bound.
```
gap2=fval1/fval2
```
<!-- CONTACT -->
## Contact

Email - luy1021@mail.ustc.edu.cn

Project Link: [https://github.com/luy1021/Myopic](https://github.com/luy1021/Myopic)



