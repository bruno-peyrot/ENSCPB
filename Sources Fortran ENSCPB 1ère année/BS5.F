
******************************************
*****      TP Informatique n≥6       *****
***** TALBOT St≈phane & PEYROT Bruno *****
******************************************



***** D≈clarations des variables

	character*1 touche
	integer c,ha,hd,prix,h,p
	parameter(max=10)
* max repr≈sente le nombre de chaque pi…ce dans le stock initial



***** D≈finition des formats

1	format(/,'Entrer l''heure d''arriv≈e: ',$)
2	format(/,'Entrer la minute d''arriv≈e: ',$)
3	format(/,/,'Entrer l''heure de d≈part: ',$)
4	format(/,'Entrer la minute de d≈part: ',$)
5	format(/,/,'Vous ¡tes rest≈: ',I2,' heure(s) et ',I2,' minute(s)')
6	format('Vous devez payer la somme de: ',I2,' Franc(s).')
7	format(/,'Introduisez ',I2,' Francs: ',$)
8	format('Les pi…ces accept≈es sont: 1F, 2F, 5F, 10F')
9	format(/,'Pi…ce non accept≈e...')
10      format(/,'Plus de monnaie, faites l''appoint SVP...')
11	format(/,'Je vous rends votre argent') 
12	format(I2,' pi…ce(s) de 10F')
13	format(I2,' pi…ce(s) de 5F')
14	format(I2,' pi…ce(s) de 2F')
15	format(I2,' pi…ce(s) de 1F')
16	format(/,/,'Merci de votre visite...')
17	format(/,'Je vous rends la monnaie')
18	format(/,/,/,'Voulez-vous continuer (O/N) ? ',$)
19	format(/,/,'Le stock final contient:')



***** Programme principal 

* Initialisation du stock de pi…ces

	n1=max
	n2=max
	n5=max
	n10=max

* Saisie des heures d'arriv≈e et de d≈part

50	write(7,1)
	read(5,*) ha
	write(7,2)
	read(5,*) ma
	write(7,3)
	read(5,*) hd
	write(7,4)
        read(5,*) md

* Calcul de la dur≈e de stationnement

	if (hd.ge.ha) then 
			  h=hd-ha 
		      else 
			  h=hd+24-ha
	end if
	if (md.ge.ma) then 
		           m=md-ma
		      else
			   h=h-1
			   m=md+60-ma
	end if

* Calcul du prix

	if (m.eq.0) then
		        prix=2*h
		    else
			prix=2*h+2
	end if

* Affichage de la dur≈e et du prix

	write(6,5) h,m
100	write(6,6) prix

* Saisie et contr¬le de la validit≈ des pi…ces introduites

	p=prix
	m1=0
	m2=0
	m5=0
	m10=0
	write(7,8)
	do while (p.gt.0)
       	     write(7,7) p
	     read(5,*) c
	     if ((c.ne.1).and.(c.ne.2).and.(c.ne.5).and.(c.ne.10)) then
		    write(7,9)
		    write(7,8)	
								   else
		    if (c.eq.1) then
				  m1=m1+1
			        else
			          if (c.eq.2) then
					        m2=m2+1
				               else
		      	                        if (c.eq.5) then
					                      m5=m5+1
				                            else
					                      m10=m10+1
			                        end if
			         end if
		    end if
		    p=p-c
	   end if
	end do

* Comptabilisation dans le stock des pi…ces introduites

	n10=n10+m10
	n5=n5+m5
	n2=n2+m2
	n1=n1+m1

* si pas de monnaie » rendre, c'est fini

	if (p.eq.0) goto 200

* Rendu de la monnaie en fonction du stock

	p=-p
	i=p/5
	if (i.gt.n5) i=n5
	j=(p-5*i)/2
	if (j.gt.n2) j=n2
	k=p-5*i-2*j
	if (k.gt.n1) then
			  write(7,10)
			  write(7,11)
			  if (m10.ne.0) write(7,12) m10
			  if (m5.ne.0) write(7,13) m5
			  if (m2.ne.0) write(7,14) m2
			  if (m1.ne.0) write(7,15) m1
			  n1=n1-m1
			  n2=n2-m2
			  n5=n5-m5
			  n10=n10-m10
			  goto 100
	 	     else		
			  n1=n1-k
			  n2=n2-j
			  n5=n5-i
			  write(7,17)
			  if (i.ne.0) write(7,13) i
			  if (j.ne.0) write(7,14) j
			  if (k.ne.0) write(7,15) k
	end if

* Merci de votre visite...

200	write(7,16)
	write(7,18)
	read(5,*) touche
	if ((touche.eq.'o').or.(touche.eq.'O')) goto 50
	write(6,19)
	write(6,12) n10
	write(6,13) n5
	write(6,14) n2
	write(6,15) n1
	end
 	


***** Fin du programme 
