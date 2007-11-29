#### Littel Helper Functions for preprocessing the Data ########


#### discretize: Discretize a Matrix to wished levels ############
discretize<-function(x,nof=10,quant=FALSE){
res<-x
ni<-dim(x)[1]
nj<-dim(x)[2]


if(quant)
{
levels<-quantile(x,seq(0+1/10,1,by=1/10))
}
else
{
mindat<-min(x)
maxdat<-max(x)
levels<-vector('integer',length=nof)
diff<-(maxdat-mindat)/nof

for(k in 1:nof)
  {
  levels[k]<-mindat+ k*diff
  }
}

for(i in 1:ni)
  {
  for(j in 1:nj)
    {
    for(k in 1:nof)
    if(x[i,j] <= levels[nof-k+1])
      {res[i,j]<-k}
    }
  }
res
}
  
  
