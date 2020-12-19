	dimension a(3,3),b(3,3),e(3,3),f(3,3),z(3,3)
	integer*2 xa,xb,ya,yb
	character*40 nom1,nom2,dat

* saisie des noms et de la date 

	write(7,1)
1	format(/,/,'Entrez le premier nom :',$)
	read(5,*) nom1
	write(7,2)
2	format('Entrez le deuxi…me nom :',$)
	read(5,*) nom2
	write(7,3)
3	format('Entrez la date :',$)
	read(5,*) dat
	write(6,4) nom1,nom2,dat
4	format(/,A40,/,A40,/,A40,/,/,/,/)
	write(6,5)
5	format(36X,'DONNEES',/,/,/)

* Lecture du fichier BS1DON.DAT et remplissage de A et B

	 open (1,file='bs1don.dat',status='old')
	 xa=1
	 xb=1
	 ya=1
	 yb=1
	 do i=1,18
		read(1,*) t
		write(6,*) t
		if (t.lt.0) then 
                                   if (xa.lt.4) then 
                                                 a(xa,ya)=t
						 ya=ya+1
						 if (ya.eq.4) then
							ya=1
							xa=xa+1
						 endif
					    else 
					         b(xb,yb)=t
						 yb=yb+1
						 if (yb.eq.4) then
							yb=1
							xb=xb+1
						 endif
                                            endif
		             else 
			         if (xb.lt.4) then 
					         b(xb,yb)=t 
						 yb=yb+1
						 if (yb.eq.4) then
							yb=1
							xb=xb+1
						 endif
					    else 
                                                 a(xa,ya)=t
						 ya=ya+1
						 if (ya.eq.4) then
							ya=1
							xa=xa+1
						 endif
                                            endif
                             endif
	end do

* Relecture des donn≈es avec un format donn≈

	rewind(1)
	do i=1,6
		read(1,10) t
		read(1,10) u
		read(1,10) v	
		write(6,11) t,u,v
10		format(F8.3)
11		format(3(E10.3))
	end do
	close (1)

* calcul de la trace de A et de B

	t1=0
	t2=0
	do i=1,3
		t1=t1+a(i,i)
		t2=t2+b(i,i)
	end do

* calcul de E et F, matrices intermediaires

	do i=1,3
		do j=1,3
			if (i.eq.j) then 
                                         e(i,j)=a(i,j)-t2
					 f(i,j)=b(i,j)-t1
				    else 
                                         e(i,j)=a(i,j)
					 f(i,j)=b(i,j)
				    endif
                 end do 
 	end do

* calcul de Z, produit de E par F 

	do i=1,3
		do j=1,3
			z(i,j)=0
		        do k=1,3
				z(i,j)=z(i,j)+e(i,k)*f(k,j)
			end do
		end do
	end do

* impression des r≈sultats

	write(6,20)
20	format(/,/,'Matrice A:',/)
	do i=1,3
		write(6,21) a(i,1),a(i,2),a(i,3)
	end do
21	format(3(E11.4))
	write(6,30)
30	format(/,/,'Matrice B:',/)
	do i=1,3
		write(6,31) b(i,1),b(i,2),b(i,3)
 	end do
31	format(3(E14.7))
	write(6,40)
40	format(/,/,'Trace des matrices A et B:',/)
	write(6,41) t1,t2
41	format('T1=',E15.8,/,/,'T2=',E15.8,/,/)
	write(6,50)
50	format('Matrice Z:',/)
	do i=1,3
		write(6,51) z(i,1),z(i,2),z(i,3)
 	end do
51	format(3(E12.5))
	end
