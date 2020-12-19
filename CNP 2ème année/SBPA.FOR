

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

        integer*2 count [C,EXTERN]
        real*4 valeur [C,EXTERN]
        dimension valeur(16)
        common nbas,ncr,icr(15),nrbs(15),hh,vm,prbs

        write(*,*)'nombre de points ?'
        read(*,*)npt
        write(*,*)'periode d''echantillonnage ?'
        read(*,*)period
        open(unit=1,file='sbpa.dat',status='old')
        nbas=5
        ncr=2
        icr(1)=2
        icr(2)=5
        do 4 i=1,5
        nrbs(i)=1
4       continue
        hh=2.
        vm=1./31.
        call kinit
        call kstart(1,3)
        count=0
        do 2 icomp=1,npt
        do 3 kk=1,3
        do 9 i=1,period
20      if(count.le.0) goto 20
        count=0
9       continue
        write(*,*)icomp,(valeur(k),k=1,3)  
        write(1,*)icomp,(valeur(k),k=1,3)
3       continue
        call sbpa
        call kwrite(0,5+prbs)
2       continue
        call kwrite(0,0)
        call kstop
        close(1)
        end

        subroutine sbpa
        common nbas,ncr,icr(15),nrbs(15),hh,vm,prbs
        ia=0
        do 11 iba=1,ncr
        ia=ia+nrbs(icr(iba))
11      continue
        ia=mod(ia,2)
        do 12 iba=nbas-1,1,-1
        nrbs(iba+1)=nrbs(iba)
12      continue
        nrbs(1)=ia
        if (ia.eq.0)ia=-1
        prbs=(ia-vm)*hh
        return
        end

