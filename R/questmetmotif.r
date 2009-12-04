#
#  Copyright (C) 2007 Sebastian Kaiser
#  Bicluster Algorithm for Questionairs based on Murali, T. & Kasif, S. Extracting conserved gene expression motifs from gene expression data Proc. Pacific Symp. Biocomputing, sullivan.bu.edu, 2003
#

## algorithm to find the biggest bicluster in questionaire (modified xmotif algorithm for questionairs)

bigquestmetmotif<-function(mat,quant,vari,ns,nd,sd,alpha)
{
d<-qnorm(1-quant,0,vari)
nr<-nrow(mat)
person<- rep(FALSE,ncol(mat))
quest<- rep(FALSE,nrow(mat))
for(i in 1:ns)
{
ri<-sample(1:nr,1)
logr<-rep(TRUE,nrow(mat))
logr[ri]<-FALSE

for(j in 1:nd)
{
D<-sample(1:nr,sd,prob=logr)

gri<-mat[ri,]
griD<-c(D,ri)
cS<-rowSums(t(mat[griD,])>=gri-d & t(mat[griD,])<=gri+d)
gij<-cS==length(griD)
if(sum(gij)>(sum(person)+1) & sum(gij)>2)
{
rri<-mat[ri,gij]
rS<-colSums(t(mat[,gij])>=rri-d & t(mat[,gij])>=rri+d)
rij<-rS==sum(gij)
if(sum(rij)>=(alpha*nr)&sum(gij)>sum(person))
{
person<-gij
quest<-rij
}

}
}

}
erg<-list(quest,person)
erg
}




## algorithm to find number biggest bicluster (Stops if all persons are in one bicluster or if no bicluster is found)

questmetmotif<-function(mat,quant,vari,ns,nd,sd,alpha,number)
{
MYCALL <- match.call()
x<-matrix(FALSE,nrow=nrow(mat),ncol=number)
y<-matrix(FALSE,nrow=number,ncol=ncol(mat))
matstore<-mat
logr<-rep(TRUE,nrow(mat))
for(i in 1:number)
{
erg<-bigquestmetmotif(mat,quant,vari,ns,nd,sd,alpha)
if(sum(erg[[1]])==0)
{break
}
else{
x[logr,i]<-erg[[1]]
y[i,]<-erg[[2]]
logr[logr][erg[[1]]]<-FALSE
mat<-matstore[logr,]
if(nrow(mat)<(sd+1))
{break}
}
}
if(i<number)
{return(BiclustResult(as.list(MYCALL),as.matrix(x[,1:(i-1)]),as.matrix(y[1:(i-1),]),(i-1),list(0)))
}
else{
return(BiclustResult(as.list(MYCALL),as.matrix(x),as.matrix(y),i,list(0)))
}
}
