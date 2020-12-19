%visualisation du contenue des fichiers res****

%load ressy
load sepd

n=size(b2r,1);
disp(['Nombre de courbe du fichier : ',num2str(n)]);

nn=input('Nombre de courbe à afficher :');

oui=input(' Afiicher toutes les courbes tapez 1 pour oui')

if nn<=n
n=nn;
end

[x,y]=meshgrid(0:0.05:1);

for i=1:n
j=((i-1)*3)+1;
w1=w1r(j:j+2,:);
b1=b1r(j:j+2);
w2=w2r(:,j:j+2);
b2=b2r(i);


%affichage de la courbe pour une sortie
i
if oui==1
figure(i);
else 
figure(1)
end
for k=1:21
 for l=1:21
  p1=[x(k,l);y(k,l)];
  zz=simuff(p1,w1,b1,'logsig',w2,b2,'purelin');
  z(k,l)=zz(1); 
 end
end

 surfc(x,y,z)

shading interp
caxis([0 1]);
view(2)
hold on
 x1=p(1,1:npts);x2=p(2,1:npts);x3=t(1,1:npts);
 plot3(x1,x2,x3,'ro');
 hold off
xlabel('Densité')
ylabel('Profondeur')
title([titre,'  precision : ',num2str(sser(i))])
pause 
end