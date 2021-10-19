clear
a1=0.85
a2=1.16
a3=3.98
b1=0.06
b2=0.08
b3=0.18
r1=0.12
r2=0.04
r3=0.02
C1=3.96
C2=4.22
C3=6.37
D1=1.00
D2=1.09
D3=1.11
theta=0.0001;
T=270;
D=5;
L=30;
b=[b1,b2,b3];
r=[r1,r2,r3];
bmax=max(b);
bmin=min(b);
rmax=max(r);

f=@(x)(-((x(1)/b1)*(a1+r1*log(D1)-log(x(1))+log(1-x(1)-x(2)-x(3)))...
    +(x(2)/b2)*(a2+r2*log(D2)-log(x(2))+log(1-x(1)-x(2)-x(3)))...
    +(x(3)/b3)*(a3+r3*log(D3)-log(x(3))+log(1-x(1)-x(2)-x(3)))...
    ));


x0=[0.01;0.01;0.01];
lb=[0;0;0];
ub=[1;1;1];
[xx1,fval1] = fmincon(f,x0,[],[],[],[],lb,ub);
gap2=1+(theta/((1-theta)*(1-theta^T)))*(1/fval1)*D^2*(r1*C1/(b1*D1)+r2*C2/(b2*D2)+r3*C3/(b3*D3))