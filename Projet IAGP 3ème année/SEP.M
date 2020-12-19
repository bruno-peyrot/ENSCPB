%
% SEPD contient les donnees expérimentales normalisé sous les noms P et T
% penser à modifier le nom du fichier de sauvegarde à la fin
%

load sepd
% si on veut utiliser seulement certains points, penser a decouper sepd


% mettre le titre voulu
titre=' Sy   25 pts  '

% varutil est le numero de la variable voulue
varutil=4

% nombre de points
npts=25

% sse demandé
pre=1e-2;


w1r=[];
w2r=[];
b1r=[];
b2r=[];
sser=[];

ter=[];
nr=[];
gsse=1;

% i va du nbre de neurones mini au nbre de neurones maxi
for i=3:3

% j decrit le nombre de tirages pour un nb de neurones donné
for j=1:10

i
j

[w1,b1,w2,b2]=initff(p(:,1:npts),i,'logsig',t(varutil,1:npts),'purelin');
tp=[0,200,pre,0.0001,0.001,10,0.1,1e10];
[w1,b1,w2,b2,te,tr]=tlm2x(w1,b1,'logsig',w2,b2,'purelin',p(:,1:npts),t(varutil,1:npts),tp);

sse=min(tr);
if sse<=gsse
 gsse=sse;
end

if sse<=pre
w1r=[w1r;w1];
w2r=[w2r,w2];
b1r=[b1r;b1];
b2r=[b2r;b2];
nr=[nr;i];
sser=[sser;sse];

ter=[ter;te];

end
end
end


% penser à modifier le nom de sauvegarde
save ressy-7 w1r b1r w2r b2r sser  ter nr titre varutil npts

%
% gsse contient le plus petit sse trouvé par les réseaux
%

gsse % plus petit sse trouvé