function m = optimal110(x)
x0=linspace(0.1,0.1,900);
lb=zeros(900,1);
ub1=ones(450,1);
ub2=2^2*ones(450,1);
ub=[ub1;ub2];
options=optimset('Maxfuneval',10000000); 
[y,ff]= fmincon(@(x) fminx(x),x0,[],[],[],[],lb,ub,@(x) fcontr(x),options);
end

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

