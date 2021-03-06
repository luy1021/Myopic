clear
a1=1.01;a2=1.29;a3=0.70;b1=0.23;b2=0.31;b3=0.28;
t=1;D=2;
X1=[];X2=[];X3=[];
P1=[];P2=[];P3=[];
PI=[];
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
