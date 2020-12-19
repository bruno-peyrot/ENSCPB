        interface to subroutine kinit [C]
        end

        interface to subroutine kstart [C] (i,j)
        integer*2 i,j
        end

        interface to subroutine kwrite [C] (i,j)
        integer*2 i
        real*4 j
        end

        interface to subroutine kstop [C]
        end

        integer*2 count[C,EXTERN]
        real*4 valeur[C,EXTERN]
        dimension valeur(16)

        open(1,file='results.dat',status='old')
        write(*,*)'nbs de points ?'
        read(*,*)npt
        write(*,*)'tension de l''echelon ?'
        read(*,*)tens
        call kinit
        call kstart(1,3)
        count=0

        do 2 icomp=1,npt
20      if (count.le.0) goto 20
        if (icomp.eq.2) call kwrite (0,tens)
        count=0
        write(1,*) icomp,(valeur(i),i=1,3)
2       continue
        call kwrite(5,0)
        call kstop
        close(1)
        end



