load boucl

for i=1:n1;

	n=nt(i)
	pr=prt(i)
	save ptest2 n pr;
	result=recom2(rc1);
	r1(i)=min(result)

end;

for i=1:n3;

	n=nt1(i)
	n2=nt2(i)
	pr=prt2(i)
	save ptest3 n n2 pr;
	result1=recom3(rc2);
	r2(i)=min(result1)

end;

save boucl n1 rc1 nt prt r1 n3 rc2 nt1 nt2 prt2 r2;

