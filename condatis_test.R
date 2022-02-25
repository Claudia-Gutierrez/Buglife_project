
hab<- raster ("data/raster/test.tif")
st<- raster ("data/raster/st.tif")
R<-100
powerthresh<- 0.25 
disp<- 1

test<-Condatis(hab, st, R, powerthresh, disp)
