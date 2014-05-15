## rejects offcuts






#' Produces a heatmap
#' @param xdat  dataset
#' @param ydat if given, a second dataset
#' @param fun function e.g. correlation to relate the datasets
#' @param ydat if given, a second dataset
#' @param ydat if given, a second dataset
#' @family experimental
#' @return A heatmap.2 plot
#' @examples Here are some examples
sigHeat <- function (xdat, ydat=xdat,fun=pseudocor,smallestprop=.75,showblocks=F,maxYlab=95,matchreverse=F,ncoll=ncol(xdat),subtitle="",maintitle="") {
  #   , showblocks, maxYlab, matchreverse, dist, maintitle, ncoll, subtitle
  library(stringr)
  #   ps=pseudocor(xdat,ydat)
  ps=do.call(pseudocor,list(xdat,ydat))
  qm=ps$sigs01
  
  library(gplots)
  adist=function(mat)dist(abs(mat))
  qnstat=ps$Ns
  qqn=format(qnstat/nrow(xdat),digits=1)
  qqn[qqn>smallestprop]=""
  anysmallprop=any(qqn!="")
  
  
  
  
  labRow=richLabs(ydat,showblocks,maxYlab)
  labCol=richLabs(xdat,showblocks,maxYlab)
  #         browser()
  if(matchreverse)distfun=adist else distfun=dist
  par(mar = rep(10,4))
  par(mai = c(1,17,7,7))
  par(pin = c(50,50))
  #           par(oma=c(3,1,2,3))
  par(plt = c(.5,.5,.6,.6))
  #   browser()
  qm=qm[,!is.na(colSums(qm,na.rm=T))]
  labCol=labCol[!is.na(colSums(qm,na.rm=T))]
  qm=qm[!is.na(rowSums(qm,na.rm=T)),]
  labRow=labRow[!is.na(rowSums(qm,na.rm=T))]
  heatmap.2(as.matrix(qm)
            
            ,col=colorpanel(50,low="deepskyblue4", mid="white", high="firebrick2")
            ,symbreaks=T
            ,margins=c(22,20)
            ,trace="both"
            ,distfun=distfun
            ,linecol="lightgrey"
            ,tracecol=NULL,
            cellnote=format(ps$estimate,digits=1)
            ,notecol="darkgrey",notecex=1.7
            ,key=F,keysize=.7
            ,labRow=labRow
            ,labCol=labCol
            ,sepcolor="gray"    
            ,colsep=1:(ncol(qm)+1)
            ,rowsep=1:nrow(qm)
            ,sepwidth=c(0.01,0.01)
            ,main=maintitle
            ,cexRow = 0.25 + 1/1.6*log10(ncoll)
            ,cexCol = 0.25 + 1/1.6*log10(ncoll)
            ,lwid=c(2,8)
            ,lhei=c(2,10)
            ,na.color="gray"
            ,revC=T
            ,xlab=paste0(subtitle,"Red cells show significant connections between row and column, blue cells show significant negative connection.\n Stronger colours mean stronger connection. Rows and columns with similar patterns of connections are grouped together into 'trees' (at the sides).",ifelse(anysmallprop,paste0("\n  - numbers in green show proportion of valid N if <",smallestprop),""))
  )
}


#' Applies \code{otest} to each pair of vectors from two datasets, or within a single dataset
#' @param xdat  dataset
#' @param ydat if given, a second dataset
#' @family experimental
#' @return list of 3 matrices giving p-values, estimates and Ns
#' @examples Here are some examples
omnicor=function(xdat,ydat=xdat){
  
  sigs=outerF(xdat,ydat,otest)
  otestp=function(x,y)attr(otest(x,y),"PRE")
  estimates=outerF(xdat,ydat,otestp)
  methods=outerF(xdat,ydat,function(x,y)attr(otest(x,y),"method"))
  Ns=outerF(xdat,ydat,function(x,y)attr(otest(x,y),"N"))
  
  
  list(sigs01=sigs01,sigs=sigs,Ns=Ns,estimates=estimates,directions=directions)
}

#' Applies \code{otest} to each pair of vectors from two datasets, or within a single dataset
#' @param xdat  dataset
#' @param ydat if given, a second dataset
#' @family experimental
#' @return list of 3 matrices giving p-values, estimates and Ns
#' @examples Here are some examples
pseudocor=function(xdat,ydat=xdat){
  sigs=data.frame(matrix(ncol=ncol(xdat),nrow=ncol(ydat)))
  estimates=data.frame(matrix(ncol=ncol(xdat),nrow=ncol(ydat)))
  Ns=data.frame(matrix(ncol=ncol(xdat),nrow=ncol(ydat)))
  
  for(ny in 1:ncol(ydat)){
    for(nx in 1:ncol(xdat)){
      mystat=otest(xdat[,nx],ydat[,ny])
      sigs[ny,nx]=mystat
      #       warning(paste0(nx,ny,mystat,sep=":"))
      #        (
      tmp=attr(mystat,"estimate")
      if(!is.null(tmp)) estimates[ny,nx]=tmp
      #           stop(tmp)
      tmp=attr(mystat,"N")
      if(!is.null(tmp)) Ns[ny,nx]=tmp
      #         warning(sigs[ny,nx])
    }
  }
  #   browser()
  # so sigs is the p values, estimates is the estimate. estimates can be 0
  directions=estimates
  directions[directions>0]=1
  directions[directions<0]=-1
  sigs01=sigs
  sigs01=(1-sigs)
  #       sigs=sigs*sigs #to reduce the number of deep reds and blues
  sigs01=sigs01*directions
  list(sigs01=sigs01,sigs=sigs,Ns=Ns,estimates=estimates,directions=directions)
}


#' Same as psych::dummy.code but with labels
#' @param x nominal dataset
#' @family experimental
#' @return labelled dataset
#' @examples Here are some examples
dummy.code.lab=function(x){
  library(psych)
  dd=dummy.code(x)
  if(is.null(attr(x,"label"))) attr(x,"label")=deparse(substitute(x))
  colnames(dd)=paste(attr(x,"label"),colnames(dd))
  dd=data.frame(dd)
  for(i in 1:ncol(dd)){
    attr(dd[,i],"origVar")=attr(x,"label")
  }
  dd
}