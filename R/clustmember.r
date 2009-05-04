
clustmember<-function(res,x,mid=T,Titel="Cluster Membership Graph",...)
{
minx<-min(x)
maxx<-max(x)
mycolor<-diverge_hcl(101, h = c(0, 130))
nx<-dim(res$centers)[1]
ny<-dim(x)[2]
xseq<-seq(0,1,length.out=nx+1)
xticks<-xseq[-length(xseq)]+xseq[2]/2
yseq<-seq(0,1,length.out=ny+1)
yticks<-yseq[-length(yseq)]+yseq[2]/2
midl<-(xseq[2]/6)

plot.new()
if(!mid)
  {
  for(i in 1:nx)
    {
    for(j in 1:ny)
      {
        identper <- res$cluster==i
        inclustercol<-(round(mean(x[identper,j]),2)-minx)*(100/(maxx-minx)) +1
        outclustercol<-(round(mean(x[!identper,j]),2)-minx)*(100/(maxx-minx)) +1


        rect(xseq[i], yseq[j], xticks[i], yseq[j+1], density = NULL, angle = 45,
        col = mycolor[inclustercol], border = NA, lty = par("lty"), lwd = par("lwd"),...)
        rect(xticks[i], yseq[j], xseq[i+1], yseq[j+1], density = NULL, angle = 45,
        col = mycolor[outclustercol], border = NA, lty = par("lty"), lwd = par("lwd"),...)

      }
    }
  }
else
  {
  for(i in 1:nx)
    {
    for(j in 1:ny)
      {
        identper <- res$cluster==i
        inclustercol<-(round(mean(x[identper,j]),2)-minx)*(100/(maxx-minx)) +1
        outclustercol<-(round(mean(x[!identper,j]),2)-minx)*(100/(maxx-minx)) +1
        rect(xseq[i], yseq[j], xticks[i]-midl, yseq[j+1], density = NULL, angle = 45,
        col = mycolor[inclustercol], border = NA, lty = par("lty"), lwd = par("lwd"),...)
        rect(xticks[i]-midl, yseq[j], xticks[i]+midl, yseq[j+1], density = NULL, angle = 45,
        col = mycolor[outclustercol], border = NA, lty = par("lty"), lwd = par("lwd"),...)
        rect(xticks[i]+midl, yseq[j], xseq[i+1], yseq[j+1], density = NULL, angle = 45,
        col = mycolor[inclustercol], border = NA, lty = par("lty"), lwd = par("lwd"),...)

      }
    }
  }



for(i in 1:length(xseq))
  {
  lines(c(xseq[i],xseq[i]),c(0,1))
  }

for(i in 1:length(yseq))
  {
  lines(c(0,1),c(yseq[i],yseq[i]))
  }

axis(1, at = xticks, labels = paste("Cluster",1:length(xticks)), tick = F,...)

axis(2, at = yticks, labels = colnames(x), tick = F,las=2,...)

axis(4, at = yticks, labels = colnames(x), tick = F,las=2,...)

title(Titel)
}




######## Biclustermembergraph ####
biclustmember<-function(bicResult,x,mid=T,Titel="BiCluster Membership Graph",...)
{
minx<-min(x)
maxx<-max(x)
mycolor<-diverge_hcl(101, h = c(0, 130))
nx<-dim(bicResult@NumberxCol)[1]
ny<-dim(bicResult@NumberxCol)[2]
xseq<-seq(0,1,length.out=nx+1)
xticks<-xseq[-length(xseq)]+xseq[2]/2
yseq<-seq(0,1,length.out=ny+1)
yticks<-yseq[-length(yseq)]+yseq[2]/2
midl<-(xseq[2]/6)

plot.new()
if(!mid)
  {
  for(i in 1:nx)
    {
    for(j in 1:ny)
      {
      if(bicResult@NumberxCol[i,j])
        {
        identper <- bicResult@RowxNumber[,i]
        inclustercol<-(round(mean(x[identper,j]),2)-minx)*(100/(maxx-minx)) +1
        outclustercol<-(round(mean(x[!identper,j]),2)-minx)*(100/(maxx-minx)) +1


        rect(xseq[i], yseq[j], xticks[i], yseq[j+1], density = NULL, angle = 45,
        col = mycolor[inclustercol], border = NA, lty = par("lty"), lwd = par("lwd"),...)
        rect(xticks[i], yseq[j], xseq[i+1], yseq[j+1], density = NULL, angle = 45,
        col = mycolor[outclustercol], border = NA, lty = par("lty"), lwd = par("lwd"),...)
        }
      }
    }
  }
else
  {
  for(i in 1:nx)
    {
    for(j in 1:ny)
      {
      if(bicResult@NumberxCol[i,j])
        {
        identper <- bicResult@RowxNumber[,i]
        inclustercol<-(round(mean(x[identper,j]),2)-minx)*(100/(maxx-minx)) +1
        outclustercol<-(round(mean(x[!identper,j]),2)-minx)*(100/(maxx-minx)) +1
        rect(xseq[i], yseq[j], xticks[i]-midl, yseq[j+1], density = NULL, angle = 45,
        col = mycolor[inclustercol], border = NA, lty = par("lty"), lwd = par("lwd"),...)
        rect(xticks[i]-midl, yseq[j], xticks[i]+midl, yseq[j+1], density = NULL, angle = 45,
        col = mycolor[outclustercol], border = NA, lty = par("lty"), lwd = par("lwd"),...)
        rect(xticks[i]+midl, yseq[j], xseq[i+1], yseq[j+1], density = NULL, angle = 45,
        col = mycolor[inclustercol], border = NA, lty = par("lty"), lwd = par("lwd"),...)
        }
      }
    }
  }



for(i in 1:length(xseq))
  {
  lines(c(xseq[i],xseq[i]),c(0,1))
  }

for(i in 1:length(yseq))
  {
  lines(c(0,1),c(yseq[i],yseq[i]))
  }

axis(1, at = xticks, labels = paste("Cluster",1:length(xticks)), tick = F,...)

axis(2, at = yticks, labels = colnames(x), tick = F,las=2,...)

axis(4, at = yticks, labels = colnames(x), tick = F,las=2,...)

title(Titel)
}
