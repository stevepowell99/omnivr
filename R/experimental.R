library(reshape)
options("stringsAsFactors"=F) #########!!!!!!!!!!!!!!!!!!!

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
#' @param cap the table caption
#' @param typ type of output, default is html but can also be latex
#' @param cn whether to show colnames
#' @family experimental
#' @return A matrix
#' @examples Here are some examples
xt=function(x,cap="",typ="html",cn=F,addmargins=0,label=NULL,digits=0,...){
  if(addmargins!=0)x=addmargins(x,addmargins)
  if(typ=="html")cat(paste("<h3>",cap,"</h3>",sep=""))
  print(xtable(x,include.colnames=cn,label=label,digits=digits),type="html",comment=F)
}


xtpandoc=function(tab,caplev=3,cap="",addmargins=NULL,digits=0,justify="right",...){
  library(pander)
#   browser()
  
  if(!is.null(addmargins))tab=addmargins(as.matrix(tab),addmargins,quiet=T)
#   cat("\n\n",rep("#",caplev)," ",cap,"\n",sep="")
  pandoc.table(tab,caption = cap,justify=justify
               ,emphasize.strong.cols=if(length(dim(tab))==2) ncol(tab) else NULL,emphasise.strong.rows=nrow(tab),...)
  
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
  #   browser()
  
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
loopr <- function (xdat, ydat=NULL,fun=oplot,duplicatesOK=TRUE,printnote=F,...) {
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
          #           cat(noteprint(xdat,ydat))fixme
          print(res)
          cat("\n")
          if(printnote){cat("*",attr(res,"note"),"*",sep = "")
          cat("\n")}
        }
      }
    }
  }
}

#' Loops through two lists and returns a  list of their pairs with no duplicates and no identical pairs. More general than loopr because x and y dont have to be dataframes
#' @param x list or vector
#' @param y another
#' @family experimental
#' @return Some stuff
#' @examples Here are some examples
loopr2=function(x,y,fun=NULL){
  l=c(NULL,NULL)
  for(xx in x){
    for(yy in y){
      #       browser()
      if(!identical(xx,yy)) {
        if(is.null(l)) l=rbind(l,c(xx,yy)) else 
          if(!any(apply(l,1,function(x)identical(x,c(xx,yy))))) if(!any(apply(l,1,function(x)identical(rev(x),c(xx,yy)))))  if(is.null(fun))l=rbind(l,c(xx,yy)) else fun(xx,yy)
      }
    }
  }
  library(plyr)
  if(is.null(fun))alply((l),1)
}

noteprint=function(x,y=NULL){
  cat("\n")
  #   browser()
  if(is.null(y)) yy = "single" else yy = attr(y,"label")
  if(xexists(notefile))cat(notefile[yy,attr(x,"label")])
  cat("\n")
}

olabel=function(x)attr(x,"label")



loopr1 <- function (xdat, ydat=xdat,fun=oplot,extra=NULL) {
  for (i in colnames(xdat)){
    print(do.call(fun,list(xdat[,i],extra)))
  }
}




richLabs1=function(varname,dat,my=maxYlab){
  #   browser()
  x=dat[,varname]
  
  tab=table(x)
  isPercent=F
  if(F)if("Yes" %in% (names(tab)) | "yes" %in% (names(tab))){
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
#     browser()
#   labss=lapply(names(df), function(x) {l=attr(df[var],"label");if(is.null(l)) x else x})
  labss=lapply(as.character(var), function(x) {l=attr(df[,x],"label");if(is.null(l)) x else l})
#   labss=lapply(df,attr,"label")
#   nl=names(labss)
#   labs=lapply(nl,function(x)ifelse(is.null(labss[[x]]),x,labss[[x]])) # put back the name if there is no label
#   names(labss)=nl
#   acv=as.character(var)
#   mat=match(acv,names(labss))
#   fff=unlist(unlist(labss[mat]))
# #   sapply(1:length(fff),function(x)if(is.na(fff[x]))labs[[x]] else fff[x])
#   res=unlist(ifelse(is.na(fff),labss,fff))
# cat(head(res))
  unlist(labss)
}

######################

#' PROBABLY OBSOLETE COS INCLUDED IN OTRANS Breaks out multi-choice dfs or combos of a df and a vector
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

#' doesnt touch xx or yy if they are just vectors. otherwise processes other structures into vectors.
#' @param xx  sth
#' @family experimental
#' @return Some stuff
#' @examples Here are some examples
otrans_inner=function(xx,yy=NULL,zz=NULL,xfilter=NULL,yfilter=NULL
                      ,mosaic=mosaic,usemultichoicelabs=F,table=F,reorder.x=F,reorder.y=F,tableout=F,msg=""){
  
  ### xx is a table so we need to do some pre-processing to get the xx and yy we want:
  if(table){
    if(!is.null(yy))stop("you said xx is a table but you supplied yy too")
    if(!is.null(zz))stop("you said xx is a table but you supplied zz too")
    ## it is a table of a single var:
    if(is.table(xx) & length(attr(xx,"dimnames"))==1) {      
      tt=data.frame(xx)
      xx=tt[,1]
      yy=tt[,2]
      ## it is a table of a two vars:
    } else {    
      if(is.table(xx)) if(length(attr(xx,"dimnames"))==2) xx=as.data.frame.matrix(xx) else stop("you gave me a table of more than two dimensions and I cant do that yet")
      ## it is actually a table-like dataframe or has just been transformed into one:
      xx$id=row.names(xx)
      m=melt(xx,id="id")
      xx=m[,1]    
      yy=m[,2]
      zz=as.numeric(m[,3])
      
    } 
  } 
  ### xx is a block so we need to do some pre-processing to get the xx and yy we want:
  #           browser()
  else 
    if(length(dim(xx))==2){ 
      if(is.null(yy)){  #xx is a block and there is no y
        xxblock=xx
        yyblock=yy
        m=melt(xx,id.vars = NULL )
        xx=m$variable
        xx=factor(findlabs(xx,xxblock))
        
        ##### it is a weird multichoice which could have been asked the other way round in which the same set of options is asked several times, like list your five qualifications
        if (!is.null(xfilter))if(xfilter=="list"){
          xx$id=row.names(xx)
          m=melt(xx,id="id")
          id=m$id
          xx=m$value
          stop("this does not work yet")
        }
        
        
        ### ... and no filter:
        if (!any(levels(factor(m$value)) %in% xfilter)) { 
          isblock=T
          
          #         if(usemultichoicelabs){
          #           xx=factor(xx,labels=sapply(levels(factor(xx)),function(x){
          #             richLabs1(x,xxblock,55)
          #           })
          #           )} else {
          #           }#FIXME
          
          attr(xx,"label")=attr(xxblock,"label")
          yy=m$value
        } else {
          msg=c(msg,(paste0("\nThis graphic relates only one possible answer ",xfilter," to this block of questions. So the total of the Counts may add up to more than the total number of respondents.\n")))
          if(is.null(yy)){ # just a filtered x block
            xx=xx[m$value %in% xfilter]
            #           if(usemultichoicelabs)xx=factor(xx,labels=sapply(levels(factor(xx)),function(x){
            #             richLabs1(x,xxblock,55)
            #           })) 
            attr(xx,"label")=attr(xxblock,"label")
            yy=NULL    
          } 
          ###so we want to plot a yes/no block against sth:
          else { 
            
            if(length(dim(yy))!=2) { #y is just a var
              #           browser()
              #         m=melt(data.frame(xx,yy),id.vars = colnames(xx) )
              m=melt(data.frame(xx,yy),id.vars = "yy" )
              m=m[m$value %in% xfilter,]
              xx=m$variable
              xx=factor(xx,labels=sapply(levels(factor(xx)),function(x){
                richLabs1(x,xxblock,55)
              }))
              attr(xx,"label")=attr(xxblock,"label")
              
              yy=m$yy
              yy=factor(yy) #TODO this will revert to nominal
              attr(yy,"label")=attr(yyblock,"label")
              
              #         data.frame(xx,yy) Note we don't return the values yet because we still need to do the chisquared stats
              
            } else {        #y is a block of similar vars
              stop("is y a block of similar vars")
            }
            
          }  
        }
      } 
      else { 
        #             browser()
        # ggheat:
        if(length(dim(xx))==2 & (
          (length(dim(yy))==2 & is.null(yfilter) ) 
          # or it is an xblock against a single y var
          |is.null(xfilter))) {
          library(stringr)
          xdat=xx
          
          if(length(dim(yy))==2) ydat=yy else {ydat=data.frame(yy);stop("I guess you wanted a heatmpa with just a single y factor but I cant do that yet")}
          #                 browser()
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
          #           browser()
          
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
          qmmd$y=factor(qmmd$y,levels=colnames(ydat)[hclust(dist(qm))$order])
          
          qmmd$x=findlabs(qmmd$x,xdat)
          qmmd$y=findlabs(qmmd$y,ydat)
          
          #   qmmd <- ddply(qmm, .(variable), transform)
          
          #   xx=as.numeric(qmmd$y)+.5
          xlab=attr(xdat,"label");if(is.null(xlab)) xlab="x"
          ylab=attr(ydat,"label");if(is.null(ylab)) ylab="y"
          #         browser()
          xx=qmmd$x
          yy=qmmd$y
          ll=qmmd$labels
          zz=qmmd$value
          aa=rep(1,nrow(qmmd))
          #         browser()
          
          #         data.frame(xx,yy,zz,aa,ll)
          
        } 
        #it must be a multimelt, i.e. the xx block is multichoice and yy is a vector or multichoice block:
        else {
          library(dplyr)
          if(is.null(xfilter)) stop("You wanted an oplot for a block and something else but you didn't give an xfilter for the block")
          
          da=data.frame(xx,stringsAsFactors = F)#;attr(da,"label")=attr(a,"label")
          db=data.frame(yy,stringsAsFactors = F)#not needed;attr(db,"label")=attr(b,"label")
          dan=name_rows(da)
          dbn=name_rows(db)
          ma=melt(dan,id=".rownames")
          maf=filter(ma,value %in% xfilter)
          if(length(dim(xx))==2) maf$label=findlabs(maf$variable,da) else maf$label=maf$value
          
          mb=melt(dbn,id=".rownames")
          if(is.null(yfilter))mbf=mb else mbf=filter(mb,value %in% yfilter)
          if(length(dim(yy))==2) mbf$label=findlabs(mbf$variable,db) else mbf$label=mbf$value
          #         if(length(dim(yy))!=2) mbf$variable=mbf$value
          
          
          mab=merge(maf,mbf,by=".rownames")
          res= mab[,xc("label.x label.y")] 
          attr(res[,1],"label")=attr(xx,"label")
          attr(res[,2],"label")=attr(yy,"label")
          xx=res[,1]
          yy=res[,2]
          
          #         oo=data.frame(xx,yy)
        }
      }
    } 
 
#   browser()
  oo=data.frame(xx=xx)
if(xexists(yy))if(!is.null(yy))oo=data.frame(oo,yy)
if(xexists(zz))if(!is.null(zz))oo=data.frame(oo,zz)
  return(oo)
}


otrans=function(xx,yy=NULL,zz=NULL,xfilter=NULL,yfilter=NULL
                ,mosaic=mosaic,aggregate=T,
                excludeSmall.x=0,
                excludeSmall.y=0,
                usemultichoicelabs=F,table=F,reorder.x=F,reorder.y=F,tableout=F){
  library(dplyr)
  if(!xexists(msg))msg=""
## it is a list:
if (is.list(xx) & !is.data.frame(xx)){
  
  check=unlist(lapply(xx,function(i)length(dim(i))))
  if(max(check)!=min(check)) stop ("You have given a list but they are not all the same type")
  
  tmp=lapply(1:length(xx),function(i){
    data.frame(xx=melt(xx[[i]],id=NULL),yy=paste0(i,attr(xx[[i]],"datafilename"),""))
    
  })#so if the first one is a factor the next ones have to accommodate to it 
  
  #     tmp=lapply(xx,function(i)data.frame(melt(i,id=NULL),labb(i))) #so if the first one is a factor the next ones have to accommodate to it 
  # note this works whether the members of xx are dataframes or just vectors
  o=do.call(rbind,tmp)
  
#   browser()
  if(max(check)==2){
    colnames(o)[1]="variable"
    colnames(o)[2]="xx"
    colnames(o)[3]="yy"
    if(!is.null(xfilter)) {
      o=o[o$value %in% xfilter,] 
    } else { # either value is numeric, and we want to take a mean, or it is some factor or text and we just wnat to pick one
      #         o=summarise(group_by(o,variable,labb.i.),mean(value))
      o=summarise(group_by(o,xx,yy),length(yy))
      zz=o[,3]
    }
  }
  yy=factor(o[,2])
  xx=factor(o[,1])
  attr(xx,"label")=olabel(xx[[1]])# it gets the label of the first one
#   browser()
  #     data.frame(xx,yy,zz) we don't return the value yet.
  #     yy=o[,2]
  
}  

oo=otrans_inner(xx,yy=yy,zz=zz,xfilter=xfilter,yfilter=yfilter,table=F,msg=msg,mosaic=mosaic)
xx=oo$xx
if(ncol(oo)>1)yy=oo[,2]
if(!is.null(oo$zz))zz=oo$zz

#######################################
  ### so we pre-processed blocks and lists into ggnormal form ; now we can continue with the xx and yy processing
  #######################################
p=otest(xx,yy)
# browser()
### it is just a single xx var for histograms:
  if (is.null(yy)) {
### tableout is true so we want to transform into a table, useful for e.g. map plots: 
    if(tableout){
    oo=data.frame(table(xx))
    colnames(oo)[2]="yy"
    
  } else oo=data.frame(xx)  
  }else {
  ### discrete_discrete
    if(classer(xx)!="con" & classer(yy)!="con"){ 
      ### z wasnt provided so we are going to get our statistics from the chisquared
      if(aggregate & is.null(zz)){ 
#             browser()
      tmp=na.omit(ddply(data.frame(xx,yy),xc("xx yy"),nrow))
      if(!is.null(zz)) tmp$V1=zz
      xt=xtabs(V1~xx+yy,data=tmp,drop.unused.levels=T)
      ct=chisq.test(xt)$stdres
      mr=melt(ct)
#       tmp2=merge(tmp,mr,all.x=T) gets the order mixed
      tmp2=join(tmp,mr,type="left")
      tmp2$ll=tmp2$V1#ifelse(rep(large,nrow(tmp2)),"",tmp2$V1)
      tmp2$zz=tmp2$value
      tmp2$aa=tmp2$ll 
      
      oo=(tmp2)

        if(!is.null(excludeSmall.x) | !is.null(excludeSmall.y)){
#           browser()
          x.l=data.frame(table(xx)/nrow(oo))
          df2=merge(oo,x.l)
          y.l=data.frame(table(yy)/nrow(oo))
          df2=merge(df2,y.l,by="yy")
          df3=df2[df2$Freq.x>excludeSmall.x & df2$Freq.y>excludeSmall.y,]
          df3$xx=droplevels(factor(df3$xx))
          df3$yy=droplevels(factor(df3$yy))
          oo=df3
        }

      
      
    } 
    ### zz was provided so we need to see how to summarise
    else if(aggregate){ 
      oo=na.omit(ddply(data.frame(xx=xx,yy=yy,zz),xc("xx yy"),summarise,ll=length(xx),zz=mean(zz,na.rm=T),aa=ll))
      if(max(oo$ll)==1) oo$ll = round(oo$zz,1)  #in fact there was no summarising to do, so ll is just 1 everytwhere and we will use zz instead for labels
      
    }

    if(reorder.x | reorder.y)if(!mosaic){
      ooo=oo[,xc("xx yy aa")]
        qm=cast(ooo,xx~yy)
        qm[is.na(qm)]=0
        if(reorder.x)oo$xx=factor(oo$xx,levels=levels(xx)[hclust(dist((qm)))$order])
      if(reorder.y)oo$yy=factor(oo$yy,levels=levels(yy)[hclust(dist(t(qm)))$order])
    }

  } else if(classer(xx)!="con" & classer(yy)=="con"){
#     stop("hoory")
    oo=na.omit(ddply(data.frame(xx=xx,yy=yy),xc("xx"),summarise,ll=length(xx),yy=mean(yy,na.rm=T),aa=ll))
#     if(max(oo$ll)==1) oo$ll = round(oo$zz,1)  #in fact there was no summarising to do, so ll is just 1 everytwhere and we will use zz instead for labels
  }
    ### what is left is that x is a single var and y is not null and they are not both discrete
  else  
    {
      oo=data.frame(xx,yy)
    }
  }
# browser()


attr(oo,"p")=p

oo
}
