c       programme de commande
c       *********************

        interface to subroutine kinit[C]
        end
        interface to subroutine kstart[C](i,j)
        integer*2i,j
        end
        interface to subroutine kwrite[C](i,j)
        integer*2i
        real*4j
        end
        interface to subroutine kstop[C]
        end

c       declaration des variables

        integer*2 count[C,EXTERN]
        real*4 valeur[C,EXTERN]
        integer*2 period
        dimension valeur(16),cf(4),t(3),yf(3)
        dimension b(4),y(300),w(300)

c       lecture des donnees et initialisation de la chaine de la chaine

        open(unit=1,file='result.dat',status='unknown')
        write(*,*)'nombre de points:'
        read(*,*)npt
        period=10
        cf(1)=1.564951
        cf(2)=-0.6441506
        cf(3)=0.04250293
        cf(4)=0.03669697

        write(*,*)'valeur de la consigne:'
        read(*,*)cons
        do 15 i=1,npt
15          w(i)=cons
        write(*,*)'donner la valeur de k:'
        read(*,*)ga
        ga1=-ga*1.285996
        ga2=ga*0.3969978
        call kinit
        call kstart(1,3)
        write(1,*)nvar,npt

c       initialisation des tables de mesure

        do 1 i=1,2
20         if (count.le.0) goto 20
           t(4-i)=valeur(1)
           yf(4-i)=t(4-i)
1          count=0

c       acquisition des npt points toutes les period secondes
        do 2 icomp=1,npt
           do 3 i=1,period
25      if (count.le.0) goto 25
        count=0
        t(1)=valeur(1)
        yf(1)=cf(1)*yf(2)+cf(2)*yf(3)+cf(3)*t(2)+cf(4)*t(3)
        do 4 j=1,2
           yf(4-j)=yf(3-j)
4          t(4-j)=t(3-j)
3          valeur(1)=yf(1)
        y(icomp)=valeur(1)
        cde=valeur(3)
        if (icomp.eq.1) then
            cde=cde+ga*(w(icomp)-y(icomp))
           else
           if (icomp.eq.2) then
              cde=cde-ga*(y(icomp)-w(icomp))-ga1*(y(icomp-1)-w(icomp-1))
              else 
              cde=cde-ga*(y(icomp)-w(icomp))-ga1*(y(icomp-1)-w(icomp-1))
     &       -ga2*(y(icomp-2)-w(icomp-2))
           endif
        endif

        call kwrite(0,cde)
        write(*,*)icomp,(valeur(k),k=1,3)
        write(*,*)cde
        write(1,*)valeur(1),valeur(3)
2       continue
        close(unit=1)
        call kstop
        end










