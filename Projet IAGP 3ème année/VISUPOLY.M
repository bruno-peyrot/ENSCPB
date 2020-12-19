
surfc(xx,yy,zz)
npts=9
shading interp
caxis([0 1]);
view(2)
hold on
 x1=p(1,1:npts);x2=p(2,1:npts);x3=t(1,1:npts);
 plot3(x1,x2,x3,'ro');
 hold off
xlabel('Densité')
ylabel('Profondeur')