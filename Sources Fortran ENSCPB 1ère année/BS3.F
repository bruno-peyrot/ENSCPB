
******************************************
*****      TP Informatique n≥4       *****
***** TALBOT St≈phane & PEYROT Bruno *****
******************************************


***** D≈clarations des variables

* la valeur max d≈finit le degr≈ maximum de A et de F
* si on veut un max>99, changer le I2 dans les formats
	parameter(max=20)
	dimension A(max),F(max),D(0:max)
	integer r


***** D≈finition des formats

1	format(/,/,/,'Entrez la valeur de n: (max=',I2,') ',$)
2	format(/,'Entrez la valeur de r: (max=',I2,') ',$)
3	format(/,'Entrez les coefficients du polyn¬me A:',/)
4	format('a(',I2,') = ',$)
5	format(/,'Les coefficients f(i) de F sont:')
6	format('f(',I2,') = ',F12.6)
7	format(/,'Les coefficients l(i) de L sont;')
8	format('l(',I2,') = ',F12.6)


***** Programme principal 

* Saisie des donn≈es au clavier

100	write (7,1) max
	read (5,*) n
	if (n.gt.max) goto 100
200	write (7,2) max 
	read (5,*) r
	if (r.gt.max) goto 200
	write (7,3)
	do i=1,n
	   write (7,4) i
	   read (5,*) a(i)
	end do

* Calcul des f(i) pour i compris entre 1 et n	

	f(1)=-a(1)
	do i=2,n
	   f(i)=-a(i)
	   do j=1,i-1
	      f(i)=f(i)-f(j)*a(i-j)
	   end do
	end do

* Calcul des f(i) pour i compris entre n+1 et r
* dans le cas oÀ r>n

	if (r.gt.n) then
	    	do i=n+1,r
		   f(i)=0
		   do j=1,n
		      f(i)=f(i)-a(j)*f(i-j)
		   end do
		end do
	endif

* Calcul des coefficients l(i) (i compris entre 0 et n-1)		

	do i=0,n-1
	   d(i)=0
	   do j=i+1,n
	      d(i)=d(i)-a(j)*f(r+i+1-j) 
	   end do
	end do

* Edition des r≈sultats

	write (6,5)
	do i=1,r
	   write (6,6) i,f(i)
	end do
	write (6,7)
	do i=0,n-1
	   write (6,8) i,d(i)
	end do
	end


***** Fin du programme 
