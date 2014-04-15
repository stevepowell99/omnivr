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
#' @import ggplot2 stringr reshape RColorBrewer
#' @docType package
#' @family main 
#' @name omnivr
NULL

library(ggplot2)
library(stringr)
library(reshape)
library(RColorBrewer)

xsig2=function(x) {  ##superceded by xsymnum, see below
  p=""
  if(is.na(x))NA else if (x<0.001) p= "v. high sig: p<.001" else if (x<0.01) p= "high sig: p<.01" else if (x<0.05) p= "sig: p<.05" else if (x<0.1) p= "possibly sig: p<0.1" else p="not significant"
  p
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
#' @family main
#' @return A ggplot graphic, with additional information provided as attributes.
#' @examples Here are some examples
oplot=function(xx,yy=NULL,simple=F,histlabs=T,na.rm=F,fillcolour=RColorBrewer::brewer.pal(3,mypal)[2]
                  ,mypal="YlOrBr",sizefac=15,xlablen=30,ylablen=30,sigLev=1){
  q=qplot()
  xlabb=xlabb=attr(xx,"label")
  xlabb=ifelse(is.null(xlabb),deparse(substitute(xx)),xlabb)
  ylabb=attr(yy,"label")
  ylabb=ifelse(is.null(ylabb),deparse(substitute(yy)),ylabb)
  library(rapport) #for skewness
  xlabb=(str_wrap(xlabb,xlablen)) #set them now but will change later if necc
  ylabb=(str_wrap(ylabb,ylablen))
  
  p=otest(xx,yy)
  #   if(simple)browser()
  if(is.null(yy)){
    q=zbar(xx,fillcolour,histlabs,sizefac)
    ylabb="Count"
  }  else {
    
    if(p[1]==0) stop(paste0("you got a zero significance: ",xlabb," or ",ylabb))
    
    
    else if(simple) {if(classer(xx) %in% xc("con ord") & classer(yy) %in% xc("con ord")){
      q=simpleline(xx,yy)
    } 
    else q=simplebar(xx,yy,mypal=mypal)
    ylabb="Count"
    }
    
    else {
      
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
      ,axis.text.x=element_text(angle=ifelse(classer(xx)=="con"|20>nchar(paste(names(table(xx)),collapse="")),0,-90),hjust=0,size=rel(1.8))        
      
#   ,panel.border = element_rect(size=rel(7))
#                                ,colour = 
#                                  if(!is.null(yy))paste(cut(p[1],
#                                           breaks=c(-1,.001,.005,.01,.05,1),labels=xc("red indianred1 pink2 mistyrose lightgrey"))) 
#                                    else if(is.null(yy)) ifelse(classer(xx)!="geo","orange","blue")  else "lightgrey")
      
    )
  # browser()
  if(p>sigLev) q=""
  q
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
  tmp=na.omit(ddply(data.frame(x2=xx,y2=yy),xc("x2 y2"),nrow))
  xt=xtabs(V1~x2+y2,data=tmp,drop.unused.levels=T)
  ct=chisq.test(xt)$stdres
  mr=melt(ct)
  tmp2=merge(tmp,mr,all.x=T)
  tmp2$large=tmp2$V1#ifelse(rep(large,nrow(tmp2)),"",tmp2$V1)
  q= ggplot(tmp2,aes(x=x2,y=y2,size=V1,colour=value,label=large))+
    theme(legend.position="none")+
    scale_colour_gradient2(low="blue",mid="grey",high="red")+
    geom_point(colour="lightgreen",alpha=1)+
    geom_text()+
    scale_size_area(max_size=4000/(sizefac*sizefac))
  attr(q,"note")="Bigger numbers are written bigger. \nSurprisingly large numbers are more red"
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
#' @param spv Whether to simulate p values for chi-squared tests. May take a while.
#' @export
#' @family main
#' @return The p-value, with additional information provided as attributes.
#' @examples Here are some examples
otest=function(xx,yy=NULL,level1="nom",level2="nom",spv=F,...){
  p=1
  
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
    
    havevarnames=!is.null(attr(xx,"varnames")) & !is.null(attr(yy,"varnames"))
    notsame=T;   
    notsame=!identical(xx,yy) 
#     if(!havevarnames) warning(paste0("If you don't provide varnames I can't be sure the two variables are not identical"),attr(yy,"label"),attr(yy,"label"))
    if(notsame | !havevarnames){
      
      if(min(length(which(table(xx)!=0)),length(which(table(yy)!=0)))>1)  {
        level1=classer(xx)
        level2=classer(yy)
        if(level1=="str") level1="nom"
        if(level2=="str") level2="nom"
        
        #       if(attr(xx,"ncol")==2 & attr(yy,"ncol")==9)
        if(level1 %in% xc("nom geo") & level2 %in% xc("nom geo"))    {if(class(try(chisq.test(xx,yy,...)))!="try-error"){
          pp=chisq.test(xx,factor(yy),simulate.p.value=spv,...)
          p=pp$p.value;attr(p,"method")="Chi-squared test"
          attr(p,"estimate")=pp$statistic
        }else p=1
        }
        
        else if(level1=="ord" & level2 %in% xc("nom geo")) 
        {if(class(try(kruskal.test(xx,factor(yy),...)))!="try-error"){
          pp=kruskal.test(xx,factor(yy),...)
          ppp<<-pp
          p=pp$p.value
          attr(p,"estimate")=pp$statistic
        } else {
          p=1
          attr(p,"method")="Kruskal test"
        }
        }
        
        else if(level1 %in% xc("nom geo") & level2=="ord")    
        {if(class(try(kruskal.test(yy,factor(xx),...)))!="try-error"){
          pp=kruskal.test(yy,factor(xx),...)
          p=pp$p.value;attr(p,"estimate")=pp$statistic
        } else {
          p=1
          attr(p,"method")="Kruskal test"
        }
        }
        
        else  if((level1=="ord" & level2=="ord") | (level1=="ord" & level2=="con") | (level1=="con" & level2=="ord")) {if(class(try(cor.test(as.numeric(xx),as.numeric (yy),method="spearman",...)))!="try-error") {pp=cor.test(as.numeric(xx),as.numeric (yy),method="spearman",...);p=pp$p.value;attr(p,"method")="Spearman rho.";attr(p,"estimate")=pp$estimate} else cat("not enough finite observations for Spearman")}
        
        else  if( level1=="con" & level2 %in% xc("nom geo")) {
          if(class(try(anova(lm(as.numeric(xx)~yy))))!="try-error"){
            pp=anova(lm(as.numeric(xx)~yy));p=pp$"Pr(>F)"[1];attr(p,"estimate")=pp["F value"];attr(p,"method")="ANOVA F"
          }else p=1}   
        
        else  if( level1 %in% xc("nom geo") & level2 %in% xc("con")) {
          if(class(try(anova(lm(as.numeric(yy)~xx))))!="try-error"){
            pp=anova(lm(as.numeric(yy)~xx));p=pp$"Pr(>F)"[1];attr(p,"estimate")=pp["F value"];attr(p,"method")="ANOVA F"
          }else p=1}   
        
        else  if( level1=="con" & level2 %in% xc("ord")) {
          if(class(try(anova(lm(as.numeric(xx)~yy))))!="try-error"){
            pp=anova(lm(as.numeric(xx)~yy));p=pp$"Pr(>F)"[1];attr(p,"estimate")=pp["F value"];attr(p,"method")="ANOVA F"
          }else p=1}   
        
        else  if( level1=="ord" & level2 %in% xc("con")) {
          if(class(try(anova(lm(as.numeric(yy)~xx))))!="try-error"){
            pp=anova(lm(as.numeric(yy)~xx));p=pp$"Pr(>F)"[1];attr(p,"estimate")=pp["F value"];attr(p,"method")="ANOVA F"
          }else p=1}   
        
        ##TODO think if these are the best tests
        else  if(level1=="con" & level2=="con")
          p=otest_continuous_continuous(xx,yy)
        
        
        #       else if(level1=="str" | level2 =="str") stop(paste0("You are trying to carry out stats tests for a string variable",attr(xx,"varnames")," or ",attr(yy,"varnames"),". You probably want to convert to nominal."))
        else {p=1
              attr(p,"estimate")=NULL
        }
        attr(p,"N")=nrow(na.omit(data.frame(xx,yy)))
      }
    } else {p=1;attr(p,"N")=sum(!is.na(xx))} #could put stuff here for single-var analysis
    
    if(is.na(p))p=1
    
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


otest_continuous_continuous=function(xx,yy)
{      
  #         ;
  pp=cor.test(as.numeric(xx),as.numeric(yy))
  p=pp$p.value
  attr(p,"method")="Pearson correlation"
  attr(p,"estimate")=pp$estimate
  
  
}   


#' Returns data type of a vector
#' 
#' A simple wrapper for \code{class()}.  The way \code{class()} expresses 
#' the difference between an ordinal an nominal variable is not convenient for our purposes.
#' Also allows for special data types to be set using the variable attribute \code{setlevout}.
#' @param x A vector.
#' @family main
#' @example R/examples/ex-classer.R
#' @note At the moment, integer is treated as continuous.
classer=function(x){
  y=class(x)[1]
  s=switch(EXPR=y,"integer"="con","factor"="nom","character"="str","numeric"="con","ordered"="ord","logical"="log")
  if(!is.null(attr(x,"setlevout"))) s=attr(x,"setlevout")
  s
}

