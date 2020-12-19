
******************************************
*****      TP Informatique n≥5       *****
***** TALBOT St≈phane & PEYROT Bruno *****
******************************************


***** D≈clarations des variables
	integer a,o,p,q
	dimension a(20,20),o(20),p(2,20),q(20,2)


***** D≈finition des formats

1	format(/,/,'Entrez la valeur de n: ',$)
2	format(/,/,'Entrez la matrice A:')
3	format(/,'Entrez l''≈l≈ment a(',I2,',',I2,') : ',$)
4	format(/,/,/,'La matrice A est:',/)
5 	format(I2,'   ',$)
6	format(/)
7	format(/,/,/,'R≈partition des travaux:')
8	format(/,'L''ouvrier n≥',I2,' va effectuer le travail n≥',I2)


***** Programme principal 

* Saisie de la matrice A au clavier

	write(7,1)
	read(5,*) n
	write(7,2)
	do i=1,n
	   do j=1,n
	       write(7,3)i,j
	       read(5,*)a(i,j)
	   end do
	end do

* D≈termination du tableau P

	do i=1,n
	   p(1,i)=i
	   p(2,i)=0
	   do j=1,n
	      p(2,i)=p(2,i)+a(j,i)
	   end do
	end do
	do i=1,n-1
	   min=i
	   do j=i+1,n
	      if (p(2,j).lt.p(2,min)) min=j
	   end do
	   m=p(1,i)
	   p(1,i)=p(1,min)
	   p(1,min)=m
	   m=p(2,i)
	   p(2,i)=p(2,min)
	   p(2,min)=m
	end do

* Initialisation du tableau O

	do i=1,n
	   o(i)=0
	end do

* D≈termination de l'ouvrier le plus qualifi≈ pour un travail donn≈

	do i=1,n
	   l=p(1,i)
	   do j=1,n
	      q(j,1)=j
	      q(j,2)=a(j,l)
	   end do
	   do j=1,n-1
	      max=j
	      do k=j+1,n
	         if (q(k,2).gt.q(max,2)) max=k
	      end do
	      m=q(j,1)
	      q(j,1)=q(max,1)
	      q(max,1)=m	
	      m=q(j,2)
	      q(j,2)=q(max,2)
	      q(max,2)=m
	   end do
	   j=1
1000	   if (o(q(j,1)).eq.0) then
				    o(q(j,1))=l
			       else
				    j=j+1
				    goto 1000
	   endif 
	end do

* Edition des r≈sultats

	write(6,4)
	do i=1,n
	   do j=1,n
              write(6,5) a(i,j)
	   end do
	   write(6,6)
	end do
	write(6,7)
	do i=1,n
	   write(6,8) i,o(i)
	end do
	
	end
 


***** Fin du programme 
