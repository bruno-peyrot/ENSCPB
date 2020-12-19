load donnees
load test
for i=1:2
[p(i,:),p1(i,:)]=nor(p(i,:),p1(i,:));
end
for i=1:4
[t(i,:),t1(i,:)]=nor(t(i,:),t1(i,:));
end
save sepd p t
save sept p1 t1