setClass('BiclustMethod',
         representation = representation('VIRTUAL',
         biclustFunction = 'function'))

setGeneric('biclust', function(x,method, ...){standardGeneric('biclust')})

setMethod('biclust', c('matrix','BiclustMethod'),
function(x,method, ...) {
  MYCALL<-match.call()
  ret<-method@biclustFunction(x,...)
  ret@Parameters<-c(list(Call=MYCALL,Data=x,Method=method),list(...))
  return(ret)
})

setMethod('biclust', c('matrix','function'),
function(x,method, ...) {
    method <- method()
    biclust(x,method, ...)
})

setMethod('biclust', c('matrix','character'),
function(x,method, ...) {
    method <- get(method[1], mode="function")
    biclust(x,method, ...)
})


setClass('Biclust',
         representation = representation(
           Parameters = 'list',
           RowxNumber = 'matrix',
           NumberxCol = 'matrix',
           Number = 'numeric'))

BiclustResult <- function(mypara, a, b, c) {
  return(new('Biclust', Parameters=mypara, RowxNumber=a, NumberxCol=b, Number=c))
}



setClass('BCBimax',
         contains = 'BiclustMethod',
         prototype = prototype(
           biclustFunction = function(x,minr=2,minc=2,number=100){bimaxbiclust(x,minr,minc,number)}))

BCBimax <- function() {
  return(new('BCBimax'))
}
         


setClass('BCXmotifs',
         contains = 'BiclustMethod',
         prototype = prototype(
           biclustFunction = function(x,ns=10,nd=10,sd=5,alpha=0.05,number=10){xmotifbiclust(x,ns,nd,sd,alpha,number)}))
         
BCXmotifs <- function() {
  return(new('BCXmotifs'))
}

setClass('BCCC',
         contains = 'BiclustMethod',
         prototype = prototype(
           biclustFunction = function(x,delta=1.0,alpha=1.5,number=100){ccbiclust(x,delta,alpha,number)}))
         
BCCC <- function() {
  return(new('BCCC'))
}
  
setClass('BCSpectral',
         contains = 'BiclustMethod',
         prototype = prototype(
           biclustFunction = function(x,normalization="log",numberOfEigenvalues=3,minGenes=2, minConditions=2, withinVar=1)
           {spectral(x,normalization, numberOfEigenvalues, minGenes, minConditions, withinVar)}))
         
BCSpectral <- function() {
  return(new('BCSpectral'))
}


setClass('BCPlaid',
         contains = 'BiclustMethod',
         prototype = prototype(
           biclustFunction = function(x, cluster="b", fit.model= ~ m + a + b, background=TRUE, row.release=0.7,col.release=0.7,shuffle=3, back.fit=2,max.layers=10,iter.startup=5,iter.layer=30, verbose=TRUE)
           {plaid(x, cluster, fit.model, background, row.release, col.release, shuffle, back.fit, max.layers, iter.startup, iter.layer, verbose) }))
         
BCPlaid <- function() {
  return(new('BCPlaid'))
}


###**show and summary*******************************

setMethod("show", "Biclust",
function(object)
{
    cat("\n\tAn object of class",class(object),"\n\n")
    cat("\tcall:", deparse(object@Parameters$Call,0.75*getOption("width")),
        sep="\n\t\t")
    cat("\n\tNumber of Clusters found: ",object@Number, "\n")   
    cat("\n\tFirst Cluster size:\n")
    cat("\t\tNumber of Rows:",sum(object@RowxNumber[,1]),"\n")
    cat("\t\tNumber of Columns:",sum(object@NumberxCol[1,]),"\n\n") 
})
        
setGeneric("summary")
setMethod("summary", "Biclust",
function(object)
{
    cat("\n\tAn object of class",class(object),"\n\n")
    cat("\tcall:", deparse(object@Parameters$Method,0.75*getOption("width")),
        sep="\n\t\t")
    cat("\n\tNumber of Clusters found: ",object@Number, "\n")   
    cat("\n\tCluster sizes:\n")
    for(i in 1:object@Number)
    {
    cat("\n\tCluster ",i,":\n")
    cat("\t\tNumber of Rows:",sum(object@RowxNumber[,i]),"\n")
    cat("\t\tNumber of Columns:",sum(object@NumberxCol[i,]),"\n\n")
    }
})

