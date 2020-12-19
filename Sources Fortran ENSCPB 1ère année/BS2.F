	
	dimension a(4,4),b(4,4,4),c(4,4),f(4,4),d(0:4)
	common a,b,c,d,f,n


***** Programme principal


* Saisie de la matrice A 

10      write(7,1)
1 	format(/,/,'entrer la valeur de n: ',$)
	read(5,*) n
	if ((n.gt.4).or.(n.lt.1)) then
				       goto 10
	endif
	 
	write(7,2)
2	format(/,/,'Entrer la matrice A:')
	do i=1,n
	  do j=1,n
	    write(7,3)i,j
3	    format(/,'Entrez l''≈l≈ment a(',I1,',',I1,') ',$)
	    read(5,*)a(i,j)
	  end do
	end do

* Affichage de la matrice A 

	write(6,20)
20	format(/,/,'Matrice A:',/)
	do i=1,n
	  do j=1,n
	    write(6,21)a(i,j)
21	    format (F11.4,'  ',$)
	  end do
	  write(6,22)
22	  format(/)
	end do

* Appel du sous-programme 'calcul'

	call calcul(A,n,B,D)

* Affichage des r≈sultats: matrices Bi et d(i)
 
	do l=1,n
	  write(6,30)l
30	  format(/,/,'Matrice B indice ',I1,':',/)
	  do i=1,n
	    do j=1,n
	      write(6,31)b(i,j,l)
31	      format (F11.4,'  ',$)
	    end do
	    write(6,32)
32	    format(/)
	  end do  
	  write(6,33)l,d(l)
33	  format(/,/,'D(',I1,') = ',F11.4)
	end do

* Saisie de la valeur de p

	write(7,40)
40	format(/,/,'Entrer la valeur de p: ',$)
	read (5,*) p

* calcul de la matrice inverse

	  e=p**n
	   do i=1,n
	     do j=1,n
		c(i,j)=b(i,j,n) 
	     end do
	   end do
	 do k=1,n
	    e=e+d(k)*p**(n-k)
	 end do  
	do l=(n-1),1,-1
	  do i=1,n
	    do j=1,n
	         c(i,j)=c(i,j)+(p**l)*b(i,j,(n-l))
	    end do
	  end do
	end do
 	do i=1,n
	  do j=1,n
	     f(i,j)=c(i,j)/e
	  end do
	end do

* Affichage de la matrice inverse

	write(6,50)
50	format(/,/,'Matrice inverse:',/)
	do i=1,n
	  do j=1,n
	    write(6,51)f(i,j)
51	    format (F11.4,'  ',$)
	  end do
	  write(6,52)
52	  format(/)
	end do
	end


***** sous-programme 'calcul'

	subroutine calcul(A,n,B,D)
	    dimension a(4,4),b(4,4,4),c(4,4),d(0:4)

* Initialisations

	        do i=1,n
		    do j=1,n
			c(i,j)=0
		    end do
	        end do
	    d(0)=1

* Calcul des matrices Bi et des traces d(i)

	    do i=1,n
	       do j=1,n
		   do k=1,n
		       b(j,k,i)=c(j,k)
			if (j.eq.k) then
					b(j,k,i)=b(j,k,i)+d(i-1)
		        endif
		   end do
	       end do
	       do j=1,n
		   do k=1,n
			c(j,k)=0
			  do l=1,n
			     c(j,k)=c(j,k)+b(j,l,i)*a(l,k)
			  end do
		   end do
	       end do
	       d(i)=0
	       do k=1,n
	          d(i)=d(i)+c(k,k)
	       end do
	       r=i
	       d(i)=-(1/r)*d(i)
	    end do
	    return
	    end


***** Fin du programme
