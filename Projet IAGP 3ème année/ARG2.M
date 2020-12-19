% calcul d'une matrice de coeficient pour un fit polynomiale
% matc est la matrice des coefficients
% res est la matrice du produit des x1...xn par les polynomes
%
% entree au clavier des valeurs initiale

nv=size(tab,2)
dp=input('degre du polygnome = ');
res=[];
% initialisation des donnees
% degre zero

A=zeros(1,nv);

matc=A;

% degre 1

A=eye(nv);

matc=[matc;A];

% degre sup a 2

for j=2:dp
	
	% definit un vecteur colonne de taille egale a A

	titi=ones(size(A,1),1);

	% cree une matrice B en ajoutant un vecteur a toute les colonnes
	B=[];
	for i=1:nv
		
		C=A;
		C(:,i)=A(:,i)+titi;
		B=[B;C];

	end

	%Elimine tout les doublons de B
	i=1;
	while i<size(B,1)

		V=B(i,:);
		ii=1;
		while ii+i<size(B,1)
			
			W=B(i+ii,:);
			if W==V
				
				B(i+ii,:)=[];
				ii=ii-1;
			end
			ii=ii+1;
		end
		i=i+1;
	end
	
	A=B;
	matc=[matc;A]
end

% calcul de la matrice X

titi=ones(size(tab,1),1);

for i=1:size(matc,1)
	V=titi;
	for ii=1:nv
	
		V=V.*mvn(tab(:,ii),matc(i,ii));
	end
	res(:,i)=V;
	
end

%calcul des coefficient du polygnome

coeff=inv(res'*res)*res'*y;

% comparaison entre les y theorique et les y mesure

yth=res*coeff
e=yth-y
sse=sumsqr(e)

