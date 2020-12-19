c
c       calcul des coefficients par la methode des moindres carres
c       **********************************************************

c       definition de x(k+1)

        subroutine iks(k,na,nb,entrm,sorm,x,r)
        dimension entrm(300),sorm(300),x(10)

        do 11 i=1,na
11           x(i)=sorm(k-i)
        do 12 i=1,nb
12           x(na+i)=entrm(k+1-i-r)
        return
        end

c       calcul de K(k+1)
 
        subroutine qua(x,p,ka,na,nb)
        dimension x(10),p(10,10)
        real int(10),ka(10)
        do 21 i=1,na+nb
            ka(i)=0
21          int(i)=0
        do 22 i=1,na+nb
            do 22 j=1,na+nb 
22              int(i)=int(i)+x(j)*p(j,i)
        a=0
        do 23 i=1,na+nb
23           a=a+int(i)*x(i)
        do 24 i=1,na+nb
            do 24 j=1,na+nb
24              ka(i)=ka(i)+p(i,j)*x(j)
        do 25 i=1,na+nb
25           ka(i)=ka(i)/(a+1)
        return
        end

c       calcul de p(k+1)

        subroutine paix(x,p,ka,na,nb)
        dimension x(10),p(10,10)
        real int1(10,10),int2(10,10),ka(10)
        do 31 i=1,na+nb
            do 31 j=1,na+nb
               int1(i,j)=0
31              int2(i,j)=0
        do 32 i=1,na+nb
           do 32 j=1,na+nb
32             int1(i,j)=ka(i)*x(j)
        do 33 i=1,na+nb
           do 33 j=1,na+nb
                do 33 k=1,na+nb
33                      int2(i,j)=int2(i,j)+int1(i,k)*p(k,j)
        do 34 i=1,na+nb
           do 34 j=1,na+nb
34             p(i,j)=p(i,j)-int2(i,j)
        return
        end

c       calcul de e(k+1)

        subroutine erreur(k,x,teta,sorm,e,na,nb)
        dimension x(10),teta(10),sorm(300)
        real int
        int=0
        do 41 i=1,na+nb
41           int=int+x(i)*teta(i)
        e=sorm(k)-int
        return
        end

c       calcul de teta(k+1)

        subroutine taita(teta,ka,e,na,nb)
        dimension teta(10)
        real ka(10)
        do 51 i=1,na+nb
51           teta(i)=teta(i)+ka(i)*e
        return
        end

c       programme principal

        character*4 var1,var2
        character*20 nomfichier
        dimension entr(300),sor(300),entrm(300),sorm(300),sm(300)
        dimension p(10,10),x(10),teta(10)
        real ka(10)
        real mentr,msor

        sentr=0
        ssor=0
        write(*,*) 'Nombre de points:'
        read(*,*)npt
        write(*,*) 'Valeur maximum pour l''ordre:'
        read(*,*)nmaxim
        write(*,*) 'Valeur du retard pur r:'
        read(*,*)r
        open(unit=1,file='SBPA.DAT',status='old')
        do 1 i=1,npt
1          read(1,*)nvar,sor(i),rvar,entr(i)
        do 2 i=1,npt
2          sentr=sentr+entr(i)
        do 3 i=1,npt
3          ssor=ssor+sor(i)
        mentr=sentr/npt
        msor=ssor/npt
        do 4 i=1,npt
4          entrm(i)=entr(i)-mentr
        do 5 i=1,npt
5          sorm(i)=sor(i)-msor
        do 19 na=1,nmaxim
         do 19 nb=na,nmaxim
          do 7 i=1,na+nb
           do 7 j=1,na+nb
7             p(i,j)=0
        do 8 i=1,na+nb
8          p(i,i)=1000000
        do 9 i=1,na+nb
9          teta(i)=0
        do 6 k=1+na,npt-1
           call iks(k,na,nb,entrm,sorm,x,r)
           call qua(x,p,ka,na,nb)
           call paix(x,p,ka,na,nb)
           call erreur(k,x,teta,sorm,e,na,nb)
6          call taita(teta,ka,e,na,nb)
        write(*,*)
        write(*,*) 'Valeur de na:',na
        write(*,*) 'Valeur de nb:',nb
        write(*,*) 'Coefficients du systeme:'
        do 17 i=1,na
           write(6,15)i,teta(i)
15         format(' a(' ,i1,')= ',f14.8)
17      continue
        do 18 i=1,nb
           write(6,16)i,teta(na+i)
16      format(' b(',i1,')=',f14.8)
18      continue

c       calcul du pi        
c       ************

        er=0.
        vs=0.
        do 10 i=1,3
10         sm(i)=sorm(i)
        do 11 i=3,npt
                sm(i)=0
           do 12 j=1,na
12            sm(i)=sm(i)+teta(j)*sm(i-j)
           do 11 j=1,nb 
11            sm(i)=sm(i)+teta(na+j)*entrm(i+1-j-r)                
        do 13 i=1,npt
13         er=er+(sorm(i)-sm(i))*(sorm(i)-sm(i))
        eqm=er/npt
        do 14 i=1,npt
14              vs=vs+sorm(i)*sorm(i)
        vs=vs/npt
        pi=(vs-eqm)*100./vs
19      write(*,*)'pourcentage d''info est :  ',pi
        close(unit=1)
        end
       


