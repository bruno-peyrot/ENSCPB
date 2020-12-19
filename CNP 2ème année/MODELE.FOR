        dimension s(300),e(300)

        open(unit=1,file='sbpa.dat',status='old')
        open(unit=2,file='modele.dat',status='unknown')
        do 1 i=1,195
1       read(1,*) nvar,s(i),rvar,e(i)
        ss=0
        ee=0
        do 2 i=1,195
        ss=ss+s(i)
2       ee=ee+e(i)
        ss=ss/195
        ee=ee/195
        do 4 i=1,195
        e(i)=e(i)-ee
4       s(i)=s(i)-ss
        do 3 i=4,195
        g=0.80681*s(i-1)-0.07487*s(i-2)-0.12243*e(i-2)-0.00798*e(i-3)
3       write(2,*) s(i),g
        close(1)
        close(2)
        end

