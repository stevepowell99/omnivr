docume=function(pkg) system(paste0("cd ..;rm ",pkg,".pdf;R CMD Rd2pdf ",pkg,";cd ",pkg))


#' Simple univariate and bivariate tests and plots, data type independent. 
#'
#' Provides two main functions which 
#' are both omnivores: the busy user can feed them data without needing to worry too much about data types: 
#' - nominal, ordinal, integer or continuous.
#' That is why the package is called \code{omnivr}: omnivorous Visualisation and Reporting. 
#'
#' 
#' The two main functions are:
#' \code{otest} for conducting simple tests of statistical significance
#' \code{oplot}, a minimalist wrapper for \code{ggplot2}. 
#' \code{ggplot2} is great for providing highly customisable plots. \code{oplot} makes it just a little bit quicker
#' to provide the simplest plots - bar charts, scatterplots etc - with a consistent appearance and interface across data types.
#' 
#' The main use case for \code{omnivr} is providing quick overview reports from the 
#' results of a questionnaire survey: 
#' providing tests of main variables against background, often sociodemographic, 
#' variables which are usually of mixed data type - nominal, ordinal, continuous etc.
#' \code{oplot} provides a function otest which conducts the statistical tests and 
#' provides a p-value.
#' By default, if \code{oplot} is provided with two variables, 
#' and the corresponding \code{oplot} test for 
#' those two variables is not significant, the plot is not produced, 
#' though the returned empty string does 
#' contain informative attributes.
#' @import ggplot2 stringr reshape RColorBrewer compute.es
#' @docType package
#' @family main omnivr functions 
#' @name omnivr
NULL

library(ggplot2)
library(stringr)
library(reshape)
library(RColorBrewer)
library(rapport)
library(wordcloud)

xsig2=function(x) {  ##superceded by xsymnum, see below
  p=""
  if(is.na(x))NA else if (x<0.001) p= "v. high sig: p<.001" else if (x<0.01) p= "high sig: p<.01" else if (x<0.05) p= "sig: p<.05" else if (x<0.1) p= "possibly sig: p<0.1" else p="not significant"
  p
}



#' Just a wrapper for wordcloud
#' separator element returns a 
#' @param stri A string, in quotes
#' @param sepp A string, usually a single character, to separate the elements of \code{stri}.
#' @family utility functions
#' @return A vector of strings.
#' @example R/examples/ex-xc.R
wplot=function(stri,cap="",min.freq=2,remove=NULL){
  pal <- brewer.pal(9,"BuGn")
  pal <- pal[-(1:4)]
  
  layout(matrix(c(1, 2), nrow=2), heights=c(.5, 5))
  par(mar=rep(0, 4))
  plot.new()
  text(x=0.5, y=0.5, paste0(cap,"\nwords most frequently stated"))
  ww=paste(stri,collapse=" ")
  ww=unlist(str_split(ww," "))
  ww[ww == remove]=""
  wordcloud(ww,min.freq=min.freq,colors=pal,main="Title",bg="lightgray")
}

#' Utility function which when given a string including at least one instance of the 
#' separator element returns a vector of strings.
#' When you want \code{x= c("red","blue","green") }
#' you can just type \code{x=xc("red blue green") }
#' @param stri A string, in quotes
#' @param sepp A string, usually a single character, to separate the elements of \code{stri}.
#' @family utility functions
#' @return A vector of strings.
#' @example R/examples/ex-xc.R
xc=function(stri,sepp=" ") (strsplit(stri, sepp)[[1]]) 


#' Provide univariate or bivariate ggplot graphic according to the data type of the input variables.
#'
#' @param xx A vector.
#' @param yy If provided, a second vector of the same length as xx.
#' @param simple Whether to provide just a simple bar chart or line plot.
#' @param histlabs Whether to provide Ns for bar charts.
#' @param na.rm Whether to exclude missing values.
#' @param fillcolour Fill colour for bar charts
#' @param mypal RColourBrewer palette
#' @param sizefac Font size multiplier for Ns on bar charts
#' @param xlablen What line length to split for xlab
#' @param ylablen What line length to split for ylab
#' @param sigLev Graphs with associated p-values above this value will not be printed.
#' @export
#' @family main omnivr functions
#' @return A ggplot graphic, with additional information provided as attributes.
#' @examples Here are some examples
oplot=function(xx,yy=NULL,simple=FALSE,histlabs=T,na.rm=F,fillcolour=RColorBrewer::brewer.pal(3,mypal)[2]
                  ,mypal="YlOrBr",sizefac=15,xlablen=30,ylablen=30,xbreakswrap=50,sigLev=.01,yeslabs=c("Yes","yes")){
  if(identical(xx,yy)) return(NULL)
#   browser()
#   stop("lkj")
msg=NULL
  isblock=F
  ######### what if xdat is a block
  if(length(dim(xx))==2){
    xxblock=xx
    yyblock=yy
    m=melt(xx,id.vars = NULL )
    if (!any(yeslabs %in% levels(m$value))) {
    isblock=T
    xx=m$variable
    xx=factor(xx,labels=sapply(levels(factor(xx)),function(x){
      richLabs1(x,xxblock,55)
    }))
    attr(xx,"label")=attr(xxblock,"label")
    yy=m$value
    } else {
      msg=c(msg,(("\nThis graphic relates only to the Yes answers to this block of questions. So the total of the Counts may add up to more than the total number of respondents.\n")))
      if(is.null(yy)){
        m=m[m$value %in% yeslabs,]
#       mm=aggregate(m,by=list(m$variable),FUN=length)
      xx=m$variable
      xx=factor(xx,labels=sapply(levels(factor(xx)),function(x){
        richLabs1(x,xxblock,55)
      }))
      attr(xx,"label")=attr(xxblock,"label")
      yy=NULL    
      } else { #so we want to plot a yes/no block against sth
        
        if(length(dim(yy))!=2) { #y is just a var
#           browser()
#         m=melt(data.frame(xx,yy),id.vars = colnames(xx) )
          m=melt(data.frame(xx,yy),id.vars = "yy" )
          m=m[m$value %in% yeslabs,]
          xx=m$variable
          xx=factor(xx,labels=sapply(levels(factor(xx)),function(x){
            richLabs1(x,xxblock,55)
          }))
          attr(xx,"label")=attr(xxblock,"label")
          
          yy=m$yy
          yy=factor(yy) #TODO this will revert to nominal
          attr(yy,"label")=attr(yyblock,"label")
          
        } else {        #y is a block of similar vars
          stop("is y a block of similar vars")
        }

        
      }  
    }
    
  }
  
  
  xlabb=attr(xx,"label")
  xlabb=ifelse(is.null(xlabb),deparse(substitute(xx)),xlabb)
  ylabb=attr(yy,"label")
# browser()
  ylabb=ifelse(is.null(ylabb),deparse(substitute(yy)),ylabb)
  
  # hack TODO
  if(isblock) ylabb=""
  
#   browser()
  library(rapport) #for skewness
  xlabb=(str_wrap(xlabb,xlablen)) #set them now but will change later if necc
  ylabb=(str_wrap(ylabb,ylablen))
  p=otest(xx,yy)
  #   if(simple)browser()
  if(is.null(yy) ){
    q=zbar(xx,fillcolour,histlabs,sizefac)
    ylabb="Count"
  }  else {
    if(is.null(p))stop(paste0("p failed"),xx,yy)
#     if(p[1]==0) warning(paste0("you got a zero significance: ",xlabb," or ",ylabb))
  
     
      if(simple) {if(classer(xx) %in% xc("con ord") & classer(yy) %in% xc("con ord")){
      q=simpleline(xx,yy)
    } 
    else q=simplebar(xx,yy,mypal=mypal)
    ylabb="Count"
    }
    
    else {
#       browser()
      if(classer(xx)!="con" & classer(yy)!="con"){
        q=oplot_discrete_discrete(xx,yy,sizefac)    
      }
      
      else if(classer(xx)!="con" & classer(yy)=="con"){
        q=oplot_discrete_continuous(xx,yy)
      } 
      
      else if(classer(xx)=="con" & classer(yy)!="con"){
        q=oplot_continuous_discrete(xx,yy,mypal,ylabb)
        ylabb="Proportion"
        
      }  
      else q=oplot_continuous_continuous(xx,yy)
    }
  }
  # browser()



  q=q+
#     xlab(waiver())+
    xlab(xlabb)+
    ylab(ylabb)+
    #     ggtitle(bquote(atop(.(tit), atop(italic(.(stat)), ""))))+
    ggtitle(attr(p,"phrase"))+
    theme(
      plot.title=element_text(size=rel(1.2))
      ,axis.text.y=element_text(size=rel(1.4),hjust=1)
      ,axis.title.y=element_text(size=rel(2),angle=0)
      ,axis.title.x=element_text(size=rel(2),angle=0)
      ,axis.text.x=element_text(angle=ifelse(classer(xx)=="con" | 20>nchar(paste(names(table(xx)),collapse="")),0,-90),hjust=0,size=rel(1.4))        
      

      
    )
  # browser()

#TODO a hack cos we dont have full support for integers
if(!is.null(attr(xx,"setlevout"))) if(attr(xx,"setlevout")=="int") q=q+
  scale_x_discrete(breaks=c(0:max(table(xx)))) 
#TODO a hack cos we dont have full support for integers


if(!is.null(p)) if(!is.na(p))   if(p<sigLev | is.null(yy)) if(p!=0) {
# cat("\n - ",attr(q,"note"))
  q=q+scale_x_discrete(breaks=levels(factor(xx)),labels=str_wrap(levels(factor(xx)),xbreakswrap))
  q=q+scale_y_discrete(breaks=levels(factor(yy)),labels=str_wrap(levels(factor(yy)),xbreakswrap))
  attr(q,"note")=c(attr(q,"note"),msg)
  q
}
}



oplot_continuous_discrete=function(xx,yy,mypal="YlOrBr",ylabb=""){
  #   browser()
  #   ggplot(data=data.frame(xx,yy),aes(yy,xx))+geom_violin(scale="count")+ stat_sum_df("mean_cl_boot",colour="orange")#smean.cl.boot is a very fast implementation of the basic nonparametric bootstrap for obtaining confidence limits for the population mean without assuming normality. These functions all delete NAs automatically.
  #   ggplot(data=dd,aes(x=dd[,1],y=dd[,2]))+geom_jitter(size=4,alpha=.3,colour="green",position = position_jitter(width = .2,height=.2))#+ stat_sum_df("mean_cl_boot",colour="orange")#
  ggplot(data=data.frame(xx,yy),aes(x=xx,fill=yy))+geom_density(position="fill",colour="black")+
    scale_fill_brewer(guide = guide_legend(title=ylabb)
                      ,type="seq",palette=mypal,breaks=rev(levels(yy)))
  
}

oplot_discrete_continuous=function(xx,yy){
  dd=data.frame(xx,yy)
  #   browser()
  q=ggplot(data=dd,aes(x=xx,y=yy))+
    geom_jitter(size=4,alpha=.3,colour="green",position = position_jitter(width = .01*length(table(xx)),height=.1*length(table(xx))))+
    geom_boxplot(alpha=.5, outlier.size = 3,outlier.colour = "red",colour="darkgreen",fill="orange",line_width=60)
  attr(q,"note")="The orange boxes contain the middle half of the points, and the line inside the box is the median. Red dots are outliers"
  q#   ggplot(data=dd,aes(x=xx,y=yy))+geom_boxplot(colour="red")+geom_jitter(size=4,alpha=.3,colour="green",position = position_jitter(width = .2,height=.2))
}

oplot_discrete_discrete=function(xx,yy,sizefac=9){
#   cat(attr(xx,"label"),attr(yy,"label"))
  warning(attr(xx,"label"),attr(yy,"label"))
  tmp=na.omit(ddply(data.frame(x2=xx,y2=yy),xc("x2 y2"),nrow))
  xt=xtabs(V1~x2+y2,data=tmp,drop.unused.levels=T)
  ct=chisq.test(xt)$stdres
#   browser()
  mr=melt(ct)
  tmp2=merge(tmp,mr,all.x=T)
  tmp2$large=tmp2$V1#ifelse(rep(large,nrow(tmp2)),"",tmp2$V1)
  q= ggplot(tmp2,aes(x=x2,y=y2,size=V1+5,colour=value,label=large))+
#     theme(legend.position="none")+
    scale_colour_gradient2(low="blue",mid="grey",high="red",labels=xc("red = more than expected;neutral;blue = less than expected",sep=";"),breaks=c(2,0,-2),guide= guide_legend(title = "Colour"))+
    geom_point(colour=brewer.pal(9,"BuGn")[2],alpha=1)+
    geom_text()+
    scale_size_area(max_size=4000/(sizefac*sizefac),guide="none")
  attr(q,"note")="Bigger numbers are written bigger. Surprisingly large numbers are more red and suprisingly small numbers are more blue."
  q
}

oplot_continuous_continuous=function(xx,yy){
  dd=data.frame(xx,yy)
  q=ggplot(data=dd,aes(x=xx,y=yy))+geom_jitter(size=4,alpha=.4,colour="green")+geom_smooth(colour="orange",size=1.5)+geom_smooth(method="lm",colour="red",alpha=.1,size=1)
  attr(q,"note")="The red line is the best straight line through the points, and the orange line is the best curved line"
  q
}

zbar=function(xx,fillcolour,histlabs,sizefac=9){
  ggplot(data=data.frame(xx),aes(x=xx))+geom_histogram(fill=fillcolour)+
    if(histlabs)stat_bin(aes(label=..count..), geom="text", position="identity",size=100/sizefac,colour="darkgreen") 
}

simpleline=function(xx,yy){ # to print out just a kind of multiple histogram unless both are at least ordinal, in which case a line graph
  #   if(class(xx)=="character") geom_blank() else if(length(unique(yy))>2 &(classer(xx) %in% xc("ord con")) & (classer(yy) %in% xc("ord con")))
  ggplot(data=data.frame(xx,yy),aes(xx,yy,group=1))+geom_smooth(alpha=0,size=3,colour="red")+
    theme(axis.text.y=element_text(size=12,hjust=1),axis.title.y=element_text(size=12),axis.text.x=element_text(angle=-90,hjust=0,size=6))
  #         scale_x_discrete(labels=str_wrap(levels(factor(xx)),50))+
  #       theme(legend.position="none")+
  #       ylab(xtext)+
  #       xlab(attr(xx,"label"))
} 

simplebarOld=function(xx,yy) {
  q=ggplot(data.frame(xx,yy),aes(x=xx,fill=..count..))+
    geom_bar(colour="black")+
    facet_grid(.~yy,labeller=facet_labeller)+
    #       coord_flip() +  
    #       theme(axis.text.x=element_text(angle=-90,hjust=0))+
    #             ,axis.text.y=element_text(size=12,hjust=1)
    #             ,axis.title.y=element_text(size=12))+
    #       scale_x_discrete(labels=str_wrap(levels(factor(yy)),50))+
    theme(legend.position="none")+
    scale_fill_continuous(low="#940609",high="#f66f21")
  #       ggtitle(attr(xx,"label"))
  attr(q,"note")=""
  q#   ggplot(data=dd,aes(x=xx,y=yy))+geom_boxplot(colour="red")+geom_jitter(size=4,alpha=.3,colour="green",position = position_jitter(width = .2,height=.2))
}

simplebar=function(xx,yy,mypal="YlOrBr") {
  q=ggplot(data.frame(xx,yy),aes(x=xx,fill=yy))+geom_bar(colour="black")+
    scale_fill_brewer(guide = guide_legend(title=attr(yy,"label")),type="seq",palette=mypal,breaks=rev(levels(yy)))
  attr(q,"note")=""
  q
}

facet_labeller=function(var,value){
  value=str_wrap(value,8)
}



#' Provide a simple bivariate test for two variables depending on the types of the variables. 
#' Also provides some basic statistics for a single variable
#'
#' @param xx A vector.
#' @param yy If provided, a second vector of the same length as xx.
#' @param level1 Force type of first variable
#' @param level2 Force type of second variable
#' @param spv Whether to simulate p values. At the moment only used for chi-squared tests. May take a while.
#' @export
#' @family main omnivr functions
#' @return The p-value, with additional information provided as attributes.
#' @example R/examples/ex-otest.R
otest=function(xx,yy=NULL,spv=FALSE,...){
  p=1
  warning(attr(xx,"ncol"),attr(yy,"ncol"))
  if(is.null(yy)){
    
    if(classer(xx) %in% xc("int con")){
      library(pander)
      attr(p,"description")=       paste0("Skewness=",round(skewness(xx,na.rm=T),digits=3),"; Kurtosis=",round(kurtosis(xx,na.rm=T),digits=3))
    } else 
      attr(p,"description")=       {
        ux <- unique(xx)        
        paste0("Mode=",ux[which.max(tabulate(match(xx, ux)))]) #paste0("Median=",median(xx,na.rm=T),"; 
      }
  }
  
  if(length(unique(xx))<2 | length(unique(yy))<2) p else {
    

      
      if(min(length(which(table(xx)!=0)),length(which(table(yy)!=0)))>1)  {
        level1=classer(xx)
        level2=classer(yy)
#         browser()
        if(is.null(level1)) level1="nom"
        if(is.null(level2)) level2="nom"
        if(level1=="str") level1="nom"
        if(level2=="str") level2="nom"
        
if(class(try(do.call(paste0("otest_",level1,"_",level2),list(xx,yy,spv))))=="try-error") {
  warning(paste("This test did not work",classer(xx),classer(yy)))
  
    p=NA
    attr(p,"method")="Kruskal but failed"
    attr(p,"estimate")=NA
    attr(p,"PRE")=NA
  
  } 
       else  
    
    p=do.call(paste0("otest_",level1,"_",level2),list(xx,yy,spv))
        

        attr(p,"N")=nrow(na.omit(data.frame(xx,yy)))
if(identical(xx,yy)) {attr(p,"PRE")=NA;attr(p,"estimate")=NA}
      }
    
   
  if(is.na(p) | is.nan(p))  {
    p=NA
    attr(p,"method")="Kruskal but failed"
    attr(p,"estimate")=NA
    attr(p,"PRE")=NA
  } 

    attr(p,"phrase")=paste0(
      "Significance: ",
      xsig2(p[1]),
      ". Test: ",
      paste0(attr(p,"method"),"."),
      paste0(attr(p,"description"),"\n")
      
      ," N=",paste0(attr(p,"N")),
      ". MISS=",length(xx)-attr(p,"N"))
    
    p
  }
}



otest_con_con=function(xx,yy,spv=FALSE)
{      
  #         ;
  pp=cor.test(as.numeric(xx),as.numeric(yy))
  p=pp$p.value
  attr(p,"method")="Pearson correlation"
  attr(p,"estimate")=as.vector(pp$estimate)
  attr(p,"PRE")=as.vector(pp$estimate)
#   ES=pwr.r.test(n = nrow(na.omit(data.frame(xx,yy))), r = attr(p,"estimate"), sig.level = p, alternative = c("two.sided"))
  p
}   

otest_ord_ord=function(xx,yy,spv=FALSE)
{      
  pp=cor.test(as.numeric(xx),as.numeric (yy),method="spearman")
  p=pp$p.value
  attr(p,"method")="Spearman rho."
  attr(p,"estimate")=pp$estimate
  attr(p,"PRE")=as.vector(pp$estimate) #debatable

  
#   attr(p,"ES")=chies(pp$statistic,nrow(na.omit(data.frame(xx,yy))))$r
  
  p
} 


otest_nom_nom=function(xx,yy,spv=FALSE)
{      
    pp=chisq.test(factor(xx),factor(yy),simulate.p.value=spv)
    p=pp$p.value;attr(p,"method")="Chi-squared test"
    attr(p,"estimate")=pp$statistic  
#     tab=prop.table(table(xx,yy))
#     attr(p,"ES")=chies(pp$statistic,nrow(na.omit(data.frame(xx,yy))))$r
  attr(p,"PRE")=as.vector(lambda.test(table(xx,yy),2) )#TODO check direction 2 or 1
  # Goodman and Kruskal's lambda

  p
} 


otest_ord_con=function(yy,xx,spv=FALSE)
{      
    pp=anova(lm(xx~yy))
    p=pp$"Pr(>F)"[1]
    attr(p,"estimate")=pp[1,"F value"]
    attr(p,"method")="ANOVA F"
    attr(p,"PRE")=as.vector(cor.test(as.numeric(xx),as.numeric (yy),method="spearman")$estimate) #debatable
    
  p
}   
otest_con_ord=function(xx,yy,spv=F)otest_ord_con(yy,xx,spv) #note with lm it does not make any difference which way round they are


otest_ord_nom=function(xx,yy,spv=FALSE)
{      
  pp=kruskal.test(xx,yy)
  p=pp$p.value
  attr(p,"estimate")=pp$statistic
  attr(p,"method")="Kruskal test"
  attr(p,"PRE")=as.vector(lambda.test(table(xx,yy),2)) #TODO check direction 2 or 1
  # Goodman and Kruskal's lambda. maybe not best for this case
  
  p
} 
otest_nom_ord=otest_ord_nom
otest_con_nom=otest_nom_ord
otest_nom_con=otest_nom_ord

otest_str_con= otest_nom_con
otest_str_ord= otest_nom_ord
otest_str_nom= otest_nom_nom
otest_con_str= otest_con_nom
otest_ord_str= otest_nom_ord
otest_nom_str= otest_nom_nom


#' Returns data type of a vector
#' 
#' A simple wrapper for \code{class()}.  The way \code{class()} expresses 
#' the difference between an ordinal an nominal variable is not convenient for our purposes.
#' Also allows for special data types to be set using the variable attribute \code{setlevout}.
#' @param x A vector.
#' @family main omnivr functions
#' @example R/examples/ex-classer.R
#' @note At the moment, integer is treated as continuous.
classer=function(x){
  y=class(x)[1]
  s=switch(EXPR=y,"integer"="con","factor"="nom","character"="str","numeric"="con","ordered"="ord","logical"="log")
  att=attr(x,"setlevout")
  if(!is.null(att)) if(!(att %in% xc("con nom str con ord int"))) s=att #you can't force a variable to be one of these types if it is not
  s
}



#' Wrapper for riverplot
#' 
#' A simple wrapper for \code{riverplot()}.  
#' If same values are found in each column, same colour will be used for each
#' @param m an edge matrix with just two columns, each row is one case joining a and b. 
#' e.g. mtcars[,3:4]
#' you could argue that to be consistent with oplot it should take x and y as input not a dataframe of them. but this way it is easier to generalise to more 
#' @family main omnivr functions
#' @note At the moment, integer is treated as continuous.
sankplot=function(mm,minedge=1,strw=12){
  library(riverplot)
  mo=na.omit(mm)
  m=data.frame(sapply(mo,as.character),stringsAsFactors = F)
  m=data.frame(sapply(m,str_wrap,strw),stringsAsFactors = F)
  labels=unique(unlist(m))
  col=rainbow(length(labels))
  col=colorRampPaletteAlpha(c( "#FF000033", "#00FF0033" ))(length(labels))
  concol=data.frame(labels,col,stringsAsFactors = F)
  m=data.frame(t(apply(m,1,paste,1:2)),stringsAsFactors = F)
  colnames(m)=paste0("N",1:2)
  
  edges=as.data.frame(summarise(group_by(m,N1,N2),Value=length(N1)))
  edges=edges[edges$Value>minedge,]
  nodes=data.frame(rbind(
    cbind(paste(unique(edges$N1)),1),
    cbind(paste(unique(edges$N2)),2) #have to differentiate because the package will complain if nodes in 2 cols have same name
  ),stringsAsFactors=F)
  nodes$X2=as.numeric(nodes$X2)
  nodes$labels=sapply(nodes[,1],str_sub,1,-3)
  nodesc=merge(nodes,concol,by="labels")
  colnames(nodesc)[2:3]=c("ID","x")
  # nodesc$col[nodesc$x==2]=NA
  makeRiver(nodesc,edges)
}
