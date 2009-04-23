jaccardind<-function(bicres1,bicres2){
  alle1<-bicres1@RowxNumber %*% bicres1@NumberxCol
  alle2<-bicres2@RowxNumber %*% bicres2@NumberxCol
  alle<-alle1 + alle2
  loalle<-alle>0
  loalle1<-alle1>0
  loalle2<-alle2>0
  res<- (sum(loalle1)+sum(loalle2)-sum(loalle))/sum(loalle)

  res


}
  
 