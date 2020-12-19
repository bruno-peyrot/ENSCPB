function[ntoto,ntoto2]=nor(toto,toto2)

max1=max(toto);
min1=min(toto);
d=max1-min1;
a=0.8/d;
b=0.1-0.8*min1/d;

ntoto=a*toto+b;
ntoto2=a*toto2+b;

