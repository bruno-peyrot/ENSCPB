%
% Affichage de la courbe d'erreur pour les fichiers
%

load sepd

for i=1:3
j=(i-1)*3+1;
x(i,1:3)=p(1,j:j+2);
y(i,1:3)=p(2,j:j+2);

end

x
y
