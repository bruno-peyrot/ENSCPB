
******************************************
*****      TP Informatique n≥7       *****
***** TALBOT St≈phane & PEYROT Bruno *****
******************************************



***** D≈clarations des variables

	character*(*) esc,cls,home,invid,under,normal
	parameter(nmax=100)
* nmax repr≈sente le nombre maximum de couples pouvant ¡tre trait≈s
* ne pas oublier de modifier la valeur de nmax dans les sous-programmes:
* RECHERCHE, MAXIMUM, MINIMUM, DILATATION et ERREUR
	character*1 choix
	integer h,s
	dimension er(2),p(8,3),pe(nmax),t(nmax)
	parameter(esc=char(27),cls=esc//'J',home=esc//'H')
	parameter(invid=esc//'&dB',under=esc//'&dD',normal=esc//'&d@')
* ces deux lignes d≈finissent des commandes de gestion de l'≈cran:
* esc: caract…re d'≈chappement (code ASCII: 27)
* cls: efface l'≈cran
* home: place le curseur en haut » gauche de l'≈cran
* invid: passe en mode inversion vid≈o
* under: passe en mode soulign≈
* normal: repasse en mode normal



***** D≈finition des formats

1	format(/,/,A,'PEYROT Bruno & TALBOT St≈phane',A,/,/,37X,A,
     *'TP n≥7',A)
2	format(/,/,22X,A,'Moindres carr≈s, m≈thode du simplex',A)
3	format(21X,A,'D≈termination de l''≈quation d''Antoine',A,/,/)
4 	format(/,'La lecture des donn≈es se fait » partir:')
5	format(/,12X,'A) du fichier ''bs6don.dat''')
6	format(12X,'B) du clavier')
7	format(/,'Quel est votre choix ? ',$)
8	format(/,A,'ERREUR:',A,' Fichier trop long.')
9	format(/,'Entrez le nombre de couples (t≥C, pression) : ',$)
10	format(/,A,'ERREUR:',A,' Le nombre maximum de couples est: ',I3)
11	format(/,'Veuillez entrer le couple n≥',I3,' : ',$)
12	format(/,A,'ERREUR:',A,' Il faut r≈pondre par A ou B')
13	format(/,/,'Lecture des donn≈es » partir de ''bs6don.dat'':',/) 
14	format(/,/,'Donn≈es saisies au clavier:',/)
15	format('t en ≥C: ',F9.3,'     Pression en mmHg: ',F9.3)
16	format(/,/,/,'Entrez les valeurs initiales de A,B et C: ',$)
17	format(/,'Entrez la valeur de epsilon: ',$)
18	format(/,/,/,'Les valeurs initiales sont:')
19	format(/,'A=',F12.6,/,'B=',F12.6,/,'C=',F12.6,/,'eps=',F12.6)
20	format(/,/,/,32X,A,'   R≈sultats   ',A)
21	format(/,/,A,'Equation d''Antoine » deux param…tres:',A)
22	format(/,32X,'Log10(Pe)=A+B/T')
23	format(/,/,A,'Equation d''Antoine » trois param…tres:',A)
24	format(/,30X,'Log10(Pe)=A+B/(t+C)')
25	format(/,'Le coefficient A est: ',F12.6)
26	format('Le coefficient B est: ',F12.6)
27	format('Le coefficient C est: ',F12.6)
28	format(/,'La somme des ≈carts carr≈s est: ',F12.6)
29	format(/,/,'Equation la plus pr≈cise: ',I1,' param…tres')
30	format(/,'L''≈cart des erreurs est: ',F12.6)



***** Programme principal 

* Affichage du titre

	write(7,*) home//cls
	write(6,1) under,normal,under,normal
	write(6,2) under,normal 
	write(6,3) under,normal

* Choix du mode d'introduction des donn≈es: fichier ou clavier

100	write(7,4)
	write(7,5)
	write(7,6)
101	write(7,7)
	read(5,*,err=101) choix
	if ((choix.eq.'A').or.(choix.eq.'a')) then

* Lecture des donn≈es » partir du fichier 'bs6don.dat'

	    open(UNIT=1, FILE='bs6don.dat', STATUS='old')
	    read(1,*) nexp
	    if (nexp.gt.nmax) then
	        close(1)
	        write(7,8) invid,normal
	        goto 100
	    end if
	    do i=1,nexp
	        read(1,*) t(i),pe(i)
	    end do
	    close(1)
					      else
	    if ((choix.eq.'B').or.(choix.eq.'b')) then	

* Saisie des donn≈es » partir du clavier

102		write(7,9)
		read(5,*,err=102) nexp
		if (nexp.gt.nmax) then
		    write(7,10) invid,normal,nmax
		    goto 102
		end if
		do i=1,nexp
		    write(7,11) i
		    read(5,*,err=103) t(i),pe(i)
		end do
						  else
103		write(7,12) invid,normal
		goto 100
	    end if
	end if

* Affichage du tableau de donn≈es

	write(7,*) home//cls
	if ((choix.eq.'A').or.(choix.eq.'a')) then
	    					  write(6,13)
					      else	
						  write(6,14)
	end if
	do i=1,nexp
	    write(6,15) t(i),pe(i)
	end do

* Saisie et affichage des valeurs initiales de A,B et C

104	write(7,16)
	read(5,*,err=104) a,b,c
105	write(7,17)
	read(5,*,err=105) eps
	write(6,18) 
	write(6,19) a,b,c,eps
	write(6,20) under,normal 

* Appel du sous-programme RECHERCHE pour les deux ≈quations
* et ≈dition des r≈sultats

	do n=2,3
	    do i=1,4
	        p(i,1)=a
	        p(i,2)=b
	        p(i,3)=c
	    end do
	    do i=1,3
	        p(i+1,i)=p(i+1,i)+eps
	    end do
	    call recherche(t,pe,p,n,h,s,nexp)
	    er(n-1)=erreur(t,pe,p,s,n,nexp)
	    if (n.eq.2) then
	        	     write(6,21) under,normal
	        	     write(6,22)
			else
			     write(6,23) under,normal
			     write(6,24)
	    end if
	    write(6,25) p(s,1)
	    write(6,26) p(s,2)
	    if (n.eq.3) write(6,27) p(s,3)
	    write(6,28) er(n-1)
	end do
	i=2
	if (er(1).gt.er(2)) i=3 
	write(6,29) i
	write(6,30) abs(er(1)-er(2))
	end



**** Sous-programme RECHERCHE

	subroutine recherche(t,pe,p,n,h,s,m)

	parameter(nmax=100)
	dimension p(8,3),pe(nmax),t(nmax)
	integer h,s

	h=0
	s=0
	it=0
201	call minimum(t,pe,p,n,s,m)
202	call maximum(t,pe,p,n,h,m)
	it=it+1
	if ((it.gt.1000).or.(erreur(t,pe,p,s,n,m).le.0.0005)) goto 203
	call reflexion(p,n,h)
	if ((erreur(t,pe,p,s,n,m).lt.erreur(t,pe,p,6,n,m)).and.
     *(erreur(t,pe,p,6,n,m).lt.erreur(t,pe,p,h,n,m))) then
	    do i=1,n
	        p(h,i)=p(6,i)
	    end do
	    goto 202
	end if
	if (erreur(t,pe,p,6,n,m).lt.erreur(t,pe,p,s,n,m)) then
	    call dilatation(t,pe,p,n,h,m)
	    goto 201
	end if
	call contraction(p,n)
	if (erreur(t,pe,p,8,n,m).lt.erreur(t,pe,p,s,n,m)) then
	    call dilatation(t,pe,p,n,h,m)
	    goto 201
	end if 
	if (erreur(t,pe,p,8,n,m).lt.erreur(t,pe,p,h,n,m)) then
	    do i=1,3
	        p(h,i)=p(8,i)
	    end do
	    goto 202
	endif
	call facteur(p,n,s)
	goto 201
203	call minimum(t,pe,p,n,s,m)
	return
	end



***** Sous-programme MAXIMUM

	subroutine maximum(t,pe,p,n,h,m)

	parameter(nmax=100)
	dimension p(8,3),pe(nmax),t(nmax)
	integer h

	h=1
	do i=2,n+1
	    if (erreur(t,pe,p,i,n,m).gt.erreur(t,pe,p,h,n,m)) h=i
	end do
	return
	end



***** Sous-programme MINIMUM

	subroutine minimum(t,pe,p,n,s,m)

	parameter(nmax=100)
	dimension p(8,3),pe(nmax),t(nmax)
	integer s

	s=1
	do i=1,n
	    if (erreur(t,pe,p,i,n,m).lt.erreur(t,pe,p,s,n,m)) s=i
	end do
	return
	end



***** Sous-programme REFLEXION

	subroutine reflexion(p,n,h)

	dimension p(8,3)
	integer h

	a=0.9985
	do i=1,n
	    p(5,i)=0
	    do j=1,n+1
	        if (j.ne.h) p(5,i)=p(5,i)+p(j,i) 
	    end do
	    r=n
	    p(5,i)=p(5,i)/r
	end do
	do i=1,n
	    p(6,i)=(1+a)*p(5,i)-a*p(h,i)
	end do
	return
	end



***** Sous-programme DILATATION

	subroutine dilatation(t,pe,p,n,h,m)

	parameter(nmax=100)
	dimension p(8,3),pe(nmax),t(nmax)
	integer h

	b=1.95
	do i=1,n
	    p(7,i)=b*p(6,i)+(1-b)*p(5,i)
	end do
	if (erreur(t,pe,p,6,n,m).lt.erreur(t,pe,p,7,n,m)) then
	    do i=1,n
	        p(h,i)=p(6,i)
	    end do
					              else
	    do i=1,n
	        p(h,i)=p(7,i)
	    end do
	end if
	return
	end


***** Sous-programme CONTRACTION

	subroutine contraction(p,n)

	dimension p(8,3)

	d=0.4985
	do i=1,n
	    p(8,i)=(1-d)*p(5,i)+d*p(6,i)
	end do
	return
	end



***** Sous-programme FACTEUR

	subroutine facteur(p,n,s)
	
	dimension p(8,3)
	real k
	integer s

	k=0.49
	do i=1,n+1
	    if (i.ne.s) then
	        do j=1,n
		    p(i,j)=p(i,j)+k*(p(s,j)-p(i,j))
	        end do
	    end if
	end do
	return
	end



***** Fonction ERREUR

	function erreur(t,pe,p,i,n,m)

	parameter(nmax=100)
	dimension p(8,3),pe(nmax),t(nmax)

	err=0
	do k=1,m
	    if (n.eq.2) then 
			      denom=t(k)+273.15
			else
			      denom=t(k)+p(i,3)
	    end if
	    err=err+((p(i,1)+p(i,2)/denom)-alog10(pe(k)))**2
	end do
	erreur=err
	return
	end



***** Fin du programme 
