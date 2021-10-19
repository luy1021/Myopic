function m = optimal500(x)
x0=linspace(0.1,0.1,1920);
lb=zeros(1920,1);
ub1=ones(960,1);
ub2=8^2*ones(960,1);
ub=[ub1;ub2];
options=optimset('Maxfuneval',10000000); 
[y,ff]= fmincon(@(x) fminx(x),x0,[],[],[],[],lb,ub,@(x) fcontr(x),options);
end

function ffff =fminx(x)
a1=0.58;a2=4.15;a3=3.83;a4=1.45;b1=0.29;b2=0.42;b3=0.34;b4=0.29;r1=0.08;r2=0.19;r3=0.08;r4=0.03;C1=6.03;C2=8.72;C3=8.39;C4=8.28;D1=0.95;D2=1.07;D3=0.98;D4=1.35;
D=8;f=0;P1=[];P2=[];P3=[];P4=[];
PI=[];
for t=1:4:957
f=(...
    f+(1/b1)*D*x(t)*(a1+r1*log(C1*D*x(t+960)+D1)-log(x(t))+log(1-x(t)-x(t+1)-x(t+2)-x(t+3)))...
    +(1/b2)*D*x(t+1)*(a2+r2*log(C2*D*x(t+961)+D2)-log(x(t+1))+log(1-x(t)-x(t+1)-x(t+2)-x(t+3)))...
    +(1/b3)*D*x(t+2)*(a3+r3*log(C3*D*x(t+962)+D3)-log(x(t+2))+log(1-x(t)-x(t+1)-x(t+2)-x(t+3)))...
    +(1/b4)*D*x(t+3)*(a4+r4*log(C4*D*x(t+963)+D4)-log(x(t+3))+log(1-x(t)-x(t+1)-x(t+2)-x(t+3)))...
    )
p1=(1/b1)*(a1+r1*log(C1*D*x(t+960)+D1)-log(x(t))+log(1-x(t)-x(t+1)-x(t+2)-x(t+3)));
p2=(1/b2)*(a2+r2*log(C2*D*x(t+961)+D2)-log(x(t+1))+log(1-x(t)-x(t+1)-x(t+2)-x(t+3)));
p3=(1/b3)*(a3+r3*log(C3*D*x(t+962)+D3)-log(x(t+2))+log(1-x(t)-x(t+1)-x(t+2)-x(t+3)));
p4=(1/b4)*(a4+r4*log(C4*D*x(t+963)+D4)-log(x(t+3))+log(1-x(t)-x(t+1)-x(t+2)-x(t+3)));
P1=[P1 p1];
P2=[P2 p2];
P3=[P3 p3];
P4=[P4 p4];
end
ffff=-f
P1
P2
P3
P4
end

function [cc,w]=fcontr(x)
for j=1:960
if j>120 & mod(j-1,4)==0
ii=0;
for t=j-120:4:j-1
ii=ii+x(t);
end
w(j)=ii-x(j+960);
elseif j<=120 & mod(j-1,4)==0
ii=0;
for t=1:4:j
ii=ii+x(t);
end
w(j)=ii-x(j+960)-x(j);
elseif j>121 & mod(j-2,4)==0
ii=0;
for t=j-120:4:j-1
i=ii+x(t);
end
w(j)=ii-x(j+960);
elseif j<=122 & mod(j-2,4)==0
ii=0;
for t=2:4:j-1
ii=ii+x(t);
end
w(j)=ii-x(j+960);
elseif j>123 & mod(j-3,4)==0
ii=0;
for t=j-120:4:j-1
ii=ii+x(t);
end
w(j)=ii-x(j+960);
elseif j<=123 & mod(j-3,4)==0
ii=0;
for t=3:4:j-1
ii=ii+x(t);
end
w(j)=ii-x(j+960);
elseif j>124 & mod(j-4,4)==0
ii=0;
for t=j-120:4:j-1
ii=ii+x(t);
end
w(j)=ii-x(j+960);
elseif j<=124 & mod(j-4,4)==0
ii=0;
for t=4:4:j-1
ii=ii+x(t);
end
w(j)=ii-x(j+960);
end
end
for t=1:4:957
cc(t)=x(t)+x(t+1)+x(t+2)+x(t+3)-1;
end
end

