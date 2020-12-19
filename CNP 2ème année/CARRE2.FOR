c       programme des moindres carres
c       *****************************

c       definition de x(k+1)

        subroutine iks(k,na,nb,entrm,sorm,x,r)
         dimension entrm(300),sorm(300),x(10)

         do 1 i=1,na
1           x(i)=entrm(k-i)
         do 2 i=1,nb
2           x(na+i)=sorm(k+1-i-r)
         return
         end

c       calcul de K(k+1)
 
        subroutine qua(x,p,ka,na,nb)
         dimension x(10),p(10,10)
         real int(10),ka(10)
         do 1 i=1,na+nb
            ka(i)=0
1           int(i)=0
         do 2 i=1,na+nb
            do 2 j=1,na+nb 
2              int(i)=int(i)+x(j)*p(j,i)
         a=0
         do 3 i=1,na+nb
3           a=a+int(i)*x(i)
         b=1./(a+1)
         do 4 i=1,na+nb
            do 4 j=1,na+nb
4              ka(i)=ka(i)+p(i,j)*x(j)
         do 5 i=1,na+nb
5           ka(i)=ka(i)*b
         return
         end

c       calcul de p(k+1)

        subroutine paix(x,p,ka,na,nb)
         dimension x(10),p(10,10)
         real int1(10,10),int2(10,10),ka(10)
         do 1 i=1,na+nb
            do 1 j=1,na+nb
               int1(i,j)=0
1              int2(i,j)=0
        do 2 i=1,na+nb
           do 2 j=1,na+nb
2             int1(i,j)=ka(i)*x(j)
        do 3 i=1,na+nb
           do 3 j=1,na+nb
           do 3 k=1,na+nb
3             int2(i,j)=int2(i,j)+int1(i,k)*p(k,j)
        do 4 i=1,na+nb
           do 4 j=1,na+nb
4             p(i,j)=p(i,j)-int2(i,j)
        return
        end

c       calcul de e(k+1)

        subroutine erreur(k,x,teta,entrm,e,na,nb)
         dimension x(10),teta(10),entrm(300)
         real int
         int=0
         do 1 i=1,na+nb
1           int=int+x(i)*teta(i)
         e=entrm(k)-int
         return
         end

c       calcul de teta(k+1)

        subroutine taita(teta,ka,e,na,nb)
         dimension teta(10)
         real ka(10)
         do 1 i=1,na+nb
1           teta(i)=teta(i)+ka(i)*e
        return
        end

c       programme principal

        character*4 var1,var2
        dimension entr(300),sor(300),entrm(300),sorm(300),sm(300)
        dimension p(10,10),x(10),teta(10)
        real ka(10)
        real mentr,msor

        sentr=0
        ssor=0
        open(unit=1,file='resultat.dat',status='old')
        do 1 i=1,155
1          read(1,*)nvar,entr(i),rvar,sor(i)
        do 2 i=1,155
2          sentr=sentr+entr(i)
        do 3 i=1,155
3          ssor=ssor+sor(i)
        mentr=sentr/155
        write(*,*)mentr
        msor=ssor/155
        write(*,*)msor
        do 4 i=1,155
4          entrm(i)=entr(i)-mentr
        do 5 i=1,155
5          sorm(i)=sor(i)-msor
        write(*,*) 'donner la valeur de na:'
        read(*,*)na
        write(*,*) 'donner la valeur de nb:'
        read(*,*)nb
        write(*,*) 'donner la valeur de r:'
        read(*,*)r
        do 7 i=1,na+nb
           do 7 j=1,na+nb
7             p(i,j)=0
        do 8 i=1,na+nb
8          p(i,i)=1000000
        do 9 i=1,na+nb
9          teta(i)=0
        do 6 k=5,154
           call iks(k,na,nb,entrm,sorm,x,r)
           call qua(x,p,ka,na,nb)
           call paix(x,p,ka,na,nb)
           call erreur(k,x,teta,entrm,e,na,nb)
6           call taita(teta,ka,e,na,nb)
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
        er=0
        vs=0
        do 10 i=1,3
10         sm(i)=entrm(i)
        do 11 i=3,npt
           do 12 j=1,na
12            sm(i)=sm(i)+teta(j)*sm(i-j)
           do 11 j=1,nb 
11            sm(i)=sm(i)+teta(na+j)*sorm(i+1-j-r)                
        do 13 i=1,npt
13         er=er+(entrm(i)-sm(i))*(entrm(i)-sm(i))
        eqm=er/npt
        do 14 i=1,npt
14              vs=vs+entrm(i)*entrm(i)
        vs=vs/npt
        pi=(vs-eqm)*100/vs
        write(*,*)'pourcentage d''info est :  ',pi
        close(unit=1)
        end
       


