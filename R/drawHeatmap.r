library(grid)
# Draws A as a heatmap, with rows and columns reordered as bicluster rows and
# columns
drawHeatmap=function(x, bicResult=NULL, number=NA)
  {
  if(is.null(bicResult))
    {#draw just the matrix
    n=dim(x)[1]
    m=dim(x)[2]

    #Color palette
    numColores=255*2
    gvect=c(array(255:0),array(0,dim=255))
    rvect=c(array(0,dim=255),array(0:255))
    bvect=array(0,dim=numColores)
    paleta=rgb(rvect, gvect, bvect, 255, maxColorValue=255)

     oldmai=par("mai")
     oldmar=par("mar")
     par(mai=c(0,0,0,0),mar=c(0,0,0,0))

    image(1:m,1:n, t(x), col=paleta, axes=FALSE)
    
    par(mai=oldmai, mar=oldmar)
    }
  else
    {
    if(is.na(number) || number>bicResult@Number || number<=0)
      {
      print("Error: the bicluster does not exist in the result set")
      }
    else
      {
      n=dim(x)[1]
      m=dim(x)[2]
  
      #Color palette
      numColores=255*2
      gvect=c(array(255:0),array(0,dim=255))
      rvect=c(array(0,dim=255),array(0:255))
      bvect=array(0,dim=numColores)
      paleta=rgb(rvect, gvect, bvect, 255, maxColorValue=255)
  
      oldmai=par("mai")
      oldmar=par("mar")
      par(mai=c(0,0,0,0),mar=c(0,0,0,0))
  
      bicRows=row(matrix(bicResult@RowxNumber[,number]))[bicResult@RowxNumber[,number]==T]
      bicCols=row(matrix(bicResult@NumberxCol[number,]))[bicResult@NumberxCol[number,]==T]
      image(1:m,1:n,
             t(x[c(setdiff(c(1:n),bicRows), bicRows),
                c(bicCols,setdiff(c(1:m),bicCols))]),
            col=paleta, axes=FALSE)
  
  
  
      desp=(n-length(bicRows))/n
      grid.lines(x=unit(c(0,1),"npc"),y=unit(c(desp,desp),"npc"), gp=gpar(col="yellow"))
      desp=length(bicCols)/m
      grid.lines(y=unit(c(0,1),"npc"),x=unit(c(desp,desp),"npc"), gp=gpar(col="yellow"))
      
      par(mai=oldmai, mar=oldmar)
      }
    }
  }