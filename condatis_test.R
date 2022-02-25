
hab<- raster("data/raster/test1")
hab
st<- raster("data/raster/st1")
st
R<-100
powerthresh<- 0.25 
disp<- 1

test<-Condatis(hab=hab, st=st, R=100, powerthresh=0.25, disp=1)
