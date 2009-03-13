#### Function to calcultae Quick Hierarical Biclustering from:
#### Quick Hierarchical Biclustering on Microarray Gene Expression Data
#### Liping Ji Kenneth Wei-Liang Mock Kian-Lee Tan
#### Department of Computer Science, National University of Singapore
#### Copyright Sebastian Kaiser 2007


qhbbiclust<-function(mat,t1=45, t2=65,minGen=2,minCon=2,MaxMFD=0.5){
##greate slope angle matrix1
mat2<-slopeanglematrix1(mat,t1)
##
sg<-seedgen(mat2,minGen,minCon)

mat3<-slopeanglematrix2(mat,t1,t2)



list(mat2,mat3)
}

## Calculate the first Slope Angle Matrix
slopeanglematrix1<-function(mat,t1){
l<-ncol(mat)-1
mat1<-mat[,-1]
mat2<-mat[,1:l]
atanmat<-atan(mat1-mat2)
t1rad<-pi*t1/180
grad90<-pi/2
bin1<-(atanmat > t1rad & atanmat < grad90)
bin2<-(atanmat < -t1rad & atanmat > -grad90) 
ret<-matrix(0,nrow=nrow(bin1),ncol=2*l)
ret[,seq(from=2,by=2,length=l)]<-bin1
ret[,seq(from=1,by=2,length=l)]<-bin2
ret
}


slopeanglematrix2<-function(mat,t1,t2){
l<-ncol(mat)-1
mat1<-mat[,-1]
mat2<-mat[,1:l]
atanmat<-atan(mat1-mat2)
t1rad<-pi*t1/180
t2rad<-pi*t2/180
grad90<-1/(8*pi)
bin1<-(abs(atanmat) >= t2rad & abs(atanmat) < grad90)
bin2<-(abs(atanmat) >  t1rad & abs(atanmat) < t2rad) 
ret<-matrix(0,nrow=nrow(bin1),ncol=2*l)
ret[,seq(from=2,by=2,length=l)]<-bin1
ret[,seq(from=1,by=2,length=l)]<-bin2
ret
}

seedgen<-function(mat,minGen,minCon){
ret<-mat

}
