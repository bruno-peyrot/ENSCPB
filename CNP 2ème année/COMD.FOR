
        interface to subroutine kinit[C]
        end

        interface to subroutine kstart[C](i,j)
        integer*2 i,j
        end

        interface to subroutine kwrite[C](i,j)
        integer*2 i
        real*4 j
        end  

        interface to subroutine kstop[C]
        end

c       declaration des variables

        integer*2 count[C,EXTERN]
        real*4 valeur[C,EXTERN]
        real*4 ka,ka1,ka2
        integer*2 period
        dimension valeur(16)
        dimension b(4),y(300),w(300)

c       lecture des donnees et initialisation de la chaine de la chaine

        open(unit=1,file='result.dat',status='unknown')
        write(*,*)'nombre de points:'
        read(*,*)npt
        period=13

        write(*,*)'valeur de la consigne:'
        read(*,*)cons
        do 15 i=1,npt
15          w(i)=cons
        write(*,*)'donner la valeur de k:'
        read(*,*)ka
        ka1=-ka*0.70289
        ka2=ka*0.00714
        call kinit
        call kstart(1,3)
        write(1,*)nvar,npt

c       acquisition des npt points toutes les period secondes
        do 2 icomp=1,npt
           do 3 i=1,period
25      if (count.le.0) goto 25
3       count=0
        cde=valeur(3)
        y(icomp)=valeur(1)
        if (icomp.eq.1) then
            cde=cde+ka*(w(icomp)-y(icomp))
           else
           if (icomp.eq.2) then
              cde=cde-ka*(y(icomp)-w(icomp))-ka1*(y(icomp-1)-w(icomp-1))
              else 
              cde=cde-ka*(y(icomp)-w(icomp))-ka1*(y(icomp-1)-w(icomp-1))
     &       -ka2*(y(icomp-2)-w(icomp-2))
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










