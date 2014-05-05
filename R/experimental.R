library(reshape)

xsigstars=function(tab,lt=FALSE) symnum(as.numeric(tab),corr=FALSE,cutpoints = c(0,  .001,.01,.05, .1, 1),symbols = c("***","** ","*  ",".  ","   "),lower.triangular=lt)


#' Applies function fun to all combinations of the variables in xdat and ydat 
#' @param xdat  dataset
#' @param ydat second dataset
#' @family experimental
#' @return A matrix
#' @examples Here are some examples
outerF=function(xdat,ydat=xdat,fun,...){
  sapply(xdat,function(x)
    sapply(ydat,function(y)
      do.call(fun,list(x,y,...))))
}


#' Wrapper for xtabs
#' @param x a table
#' @family experimental
#' @return A matrix
#' @examples Here are some examples
xt=function(x,cap="",typ="html",cn=F,...){
  if(typ=="html")cat(paste("<h3>",cap,"</h3>",sep=""))
  print(xtable(x),type="html",comment=F,include.colnames=cn)
}




#' Produces a ggplot heatmap
#' @param xdat  dataset
#' @param ydat if given, a second dataset
#' @param fun function e.g. correlation to relate the datasets
#' @param ydat if given, a second dataset
#' @param ydat if given, a second dataset
#' @family experimental
#' @return A heatmap.2 plot
#' @examples Here are some examples
ggheat <- function (xdat, ydat=xdat) { #,fun=pseudocor,smallestprop=.75,showblocks=F,maxYlab=95,matchreverse=F,ncoll=ncol(xdat),subtitle="",maintitle=""
  #   , showblocks, maxYlab, matchreverse, dist, maintitle, ncoll, subtitle
  library(stringr)
  
  xss=(sapply(xdat,function(x)max(table(x)))<1)
  if(any(xss)) {
#     warning(cat("Some variables have no variance: " ,attr(xdat,"label"),"variables: ",colnames(xdat)[xss]))
#     xdat=xdat[,!xss]
  }
  
  yss=(sapply(ydat,function(x)max(table(x)))<1)
  if(any(yss)) {
#     if(!identical(xdat,ydat))warning(cat("Some variables have no variance: " ,attr(ydat,"label"),"variables: ",colnames(ydat)[yss]))
    ydat=ydat[,!yss]
  }
  
  xss=(sapply(xdat,function(x)sum(table(x)>0))<2) #var takes only one value
  if(any(xss)) {
    warning(cat("Some variables have no variance: " ,attr(xdat,"label"),"variables: ",colnames(xdat)[xss]))
#     xdat=xdat[,!xss]
  }
  
  yss=(sapply(ydat,function(x)sum(table(x)>0))<2)
  if(any(yss)) {
    if(!identical(xdat,ydat))warning(cat("Some variables have no variance: " ,attr(ydat,"label"),"variables: ",colnames(ydat)[yss]))
    ydat=ydat[,!yss]
  }
  
  
  qsig=data.frame(outerF(xdat,ydat,otest))
#   warning(paste0(attr(xdat,"label"),attr(ydat,"label")))
  qm=data.frame(outerF(xdat,ydat,function(x,y)attr(otest(x,y),"PRE")))
  
  
  ######### extract rows and cols where every sigval is 1 BUT IT DOESNT QUITE WORK
#   tmp=qsig
#   qsig=qsig[rowSums(qsig)!=ncol(qsig),colSums(qsig)!=nrow(qsig)]
#   qm=qm[rowSums(tmp)!=ncol(tmp),colSums(tmp)!=nrow(tmp)]
  
#   
  # ############
  #thanks to https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
  ###############

  qmn=namerows(data.frame(qm), col.name = "y")
  qmm <- melt(qmn,id.vars = "y")
  
  qs=namerows(data.frame(qsig), col.name = "y")
  qsm <- melt(qs,id.vars = "y")
colnames(qsm)[3] = "sig"

qmmd=merge(qmm,qsm,by = xc("y variable"))


qmmd$value[qmmd$value==1]=NA


qmmd$labels=paste0(round(qmmd$value,digits=2),xsigstars(qmmd$sig))
qmmd$labels[is.na(qmmd$value)]=""
qmmd$labface=ifelse(qmmd$sig<.001,2,1)
qmmd$x=factor(qmmd$variable,levels=colnames(xdat)[hclust(dist(t(qm)))$order])
# browser()
qmmd$y=factor(qmmd$y,levels=colnames(ydat)[hclust(dist(qm))$order])
  
#   qmmd <- ddply(qmm, .(variable), transform)
  
#   xx=as.numeric(qmmd$y)+.5
xlab=attr(xdat,"label");if(is.null(xlab)) xlab="x"
ylab=attr(ydat,"label");if(is.null(ylab)) ylab="y"
  
  p <- ggplot(qmmd, aes(x, y,label=labels)) +
  geom_tile(aes(fill=value),colour = "white") +
  geom_text(size=rel(3),aes(fontface=labface),colour=brewer.pal(name = "Greens",5)[5])+
  scale_fill_gradient2(low = "steelblue",mid="white",high = brewer.pal(name = "Reds",3)[3],limits=c(-1,1),na.value=brewer.pal(name = "YlOrRd",3)[1])


p=p + theme_bw() + 
#     labs(x = "",y = "") +
  scale_x_discrete(name=xlab,expand = c(0, 0),labels=sapply(levels(factor(qmmd$x)),function(x){
    richLabs1(x,xdat,55)
  }))+
  scale_y_discrete(name=ylab,expand = c(0, 0),labels=sapply(levels(factor(qmmd$y)),function(x){
      richLabs1(x,ydat,55)
    })) + 
    theme(title=element_text(size=rel(.65)),legend.position = "none",axis.ticks = element_blank(), axis.text.x = element_text(angle = 280, hjust = 0, colour = "grey50"))
#   p+geom_rug(aes(y=xx,size=xx/11+10),sides="l")+geom_rug(aes(x=vv+.5,size=vv),sides="b")
#+geom_segment(aes(y=.2,yend=.5,x=xx,xend=xx,size=value))
attr(p,"note")=paste0("Relationships between question groups: `",xlab,"` and ",if(identical(xdat,ydat)) "itself. " else paste0(" `",ylab,"`. "),
                      "The strongest positive relationships (nearly +1) are coloured dark red \nand the strongest negative relationships (nearly -1) are coloured dark blue.",
                      "\nQuestions which have similar patterns of relationships are grouped together."
                      ,"\nStars show statistically significant relationships; more stars mean more significant."
                      ,if(identical(xdat,ydat)) "\nWhere a question is compared with itself, the square is coloured orange."
                      ,"\nThe numbers at the beginning of the question labels are the serial numbers of the questions.")
p
}


#' Loops through one or two datasets
#' @param xdat  dataset
#' @param ydat if given, a second dataset
#' @param fun function e.g. correlation to relate the datasets
#' @param extra named vector of arguments to fun e.g. 
#' @param extra named vector of arguments to fun e.g. 
#' @family experimental
#' @return Some stuff
#' @examples Here are some examples
loopr <- function (xdat, ydat=NULL,fun=oplot,duplicatesOK=TRUE,...) {
  for (x in (1:ncol(xdat))){
    i=xdat[,x]
    if(is.null(ydat)){
    res=do.call(fun,list(i,...))
    if(!is.null(res))
      {print(res)
    cat(attr(res,"note"))
    }
      
    } else for (y in 1:ncol(ydat)){
      j=ydat[,y]
    if(!(!duplicatesOK & y>x) )
      {
      res=do.call(fun,list(i,j,...))
    if(!is.null(res))       {
      cat(noteprint(xdat,ydat))
      print(res)
      cat("\n")
      cat("*",attr(res,"note"),"*",sep = "")
      cat("\n")
    }
    }
  }
}
}


noteprint=function(x,y=NULL){
  cat("\n")
#   browser()
  if(is.null(y)) yy = "single" else yy = attr(y,"label")
  cat(notefile[yy,attr(x,"label")])
  cat("\n")
}





loopr1 <- function (xdat, ydat=xdat,fun=oplot,extra=NULL) {
  for (i in colnames(xdat)){
    print(do.call(fun,list(xdat[,i],extra)))
  }
}




richLabs1=function(varname,dat,my=maxYlab){
  x=dat[,varname]
    tab=table(x)
    isPercent=F
    if("Yes" %in% (names(tab)) | "yes" %in% (names(tab))){
      #             browser()
      isPercent=T
      percent=round(100*tab[names(tab)=="Yes" | names(tab)=="yes"]/length(na.omit(x)))
    }
    av=mean(as.numeric(x),na.rm=T)
#     firstBlock=function(y)names(which(!is.na(attributes(y)[blocks]))[1])
    pp=paste0(
#       ifelse(sb,paste0(firstBlock(x),": "),"")
#       ,
      
      attr(x,"ncol"),
      str_sub(classer(x),1,1),
      ": ",
      if(!is.null(attr(x,"label"))) str_sub(attr(x,"label"),1,my) else str_sub(x,1,my),
      " (",
      if(isPercent) paste0(percent,"%") else if(classer(x) %in% xc("con int ord") | classer(x)=="ord" & length(tab)>2) paste0("av=",ifelse(av<.01,"<.01",format(av,digits=2)))
      else if(classer(x) %in% xc("ord nom")) {
        if(length(tab)>2) paste0("levels=",length(tab))
        else paste0("'",names(tab)[2],"'=",format(tab[2]*100/sum(tab),digits=2),"%")
      },
      ")"
      )
    str_wrap(pp,50)
  }


######################

#' More robust test of whether something is NA or null.
#' Should not give an error or NA, whatever x is.
#' @param x The object to be tested. May not even exist.
#' @param y
#'  
#' true if missing or null or y, otherwise false. NOTE IT GIVES F IF IT IS ANY DATA FRAME, EVEN AN EMPTY ONE
xmb=function(x,y="") if(!xexists(x))T else{ if(length(x)==0) TRUE  else if(class(x)=="data.frame") FALSE else if(is.na(x)) TRUE else if(is.null(x)) TRUE  else if(x==y) TRUE else FALSE}

xexists=function(x)class(try(class(x),silent=T))!="try-error"

######################

#' Given a vector of variablenames, returns the variable labels in df
#' Useful for input into sankplot
#' @param var
#' @param df
#'  
#'  
findlabs=function(var,df){
  labs=sapply(df,attr,"label") 
  mat=match(as.character(var),names(labs))
  fff=unlist(labs[mat])
  ifelse(is.na(fff),labs,fff)
  
  }

######################

#' Breaks out multi-choice dfs or combos of a df and a vector
#' Useful for input into sankplot
#' @param x 
#' @param y
#'  
#'  
multimelt=function(a,b,nochars=xc("No no"),uselabs=T){
  library(dplyr)
  
  da=data.frame(a,stringsAsFactors = F)#;attr(da,"label")=attr(a,"label")
  db=data.frame(b,stringsAsFactors = F)#not needed;attr(db,"label")=attr(b,"label")
  dan=name_rows(da)
  dbn=name_rows(db)
  ma=melt(dan,id=".rownames")
  maf=filter(ma,!(value %in% nochars)) 
  if(length(dim(a))==2) maf$label=findlabs(maf$variable,da) else maf$label=maf$value
  
  mb=melt(dbn,id=".rownames")
  mbf=filter(mb,!(value %in% nochars)) 
  if(length(dim(b))==2) mbf$label=findlabs(mbf$variable,db) else mbf$label=mbf$value
  
  
  mab=merge(maf,mbf,by=".rownames")
  if(uselabs) res= mab[,xc("label.x label.y")] else res=mab[,xc("variable.x variable.y")] 
  attr(res[,1],"label")=attr(a,"label")
  attr(res[,2],"label")=attr(b,"label")
  res
  }

