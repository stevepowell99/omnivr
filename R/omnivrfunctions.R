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
#' @param xx A vector, dataframe, table or list.
#' @param yy If provided, a second vector of the same length as xx.
#' @param zz If provided, the statistic to be plotted.
#' @param ll If provided, the label to be plotted.
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
oplot=function(xx,yy=NULL,zz=NULL,ll=NULL,simple=FALSE,histlabs=T,na.rm=F,fillcolour=RColorBrewer::brewer.pal(3,mypal)[2]
               ,mypal="YlOrBr",sizefac=15,reorder.x=F,reorder.y=F,xlabb=NULL,ylabb=NULL,xlablen=30,ylablen=30,
               xbreakswrap=50,ybreakswrap=50,sigLev=.01,xfilter=NULL,yfilter=NULL,
               usemultichoicelabs=F,percent=F,mosaic=F,graph=F,pie=F,
               position="fill",
               nfacetcol=5,
               img=NULL,
               printSig=F,excludeSmall.x=.1,excludeSmall.y=.1,...){
  # browser()
  #   if(identical(xx,yy)) return(NULL)
  #   stop("lkj")
  msg=NULL
  isblock=F
  ######### what if xdat is a block
  
  #   xlabb=ifelse(is.null(xlabb),labb(xx))
  #   ylabb=ifelse(is.null(ylabb),labb(yy))
  ylabb=attr(yy,"label")
  ylabb=ifelse(is.null(ylabb),deparse(substitute(yy)),ylabb)
  xlabb=attr(xx,"label")
  xlabb=ifelse(is.null(xlabb),deparse(substitute(xx)),xlabb)
  
  o=otrans(xx,yy,zz=zz,xfilter=xfilter,yfilter=yfilter
           ,aggregate=!mosaic,mosaic=mosaic,
           excludeSmall.x=excludeSmall.x,
           excludeSmall.y=excludeSmall.y,
           usemultichoicelabs=usemultichoicelabs,reorder.x=reorder.x,reorder.y=reorder.y,...)
#   browser()
  ## total hack for mosaic
  p=attr(o,"p")
  xx=o$xx
  yy=o$yy
  if(!is.null(o$zz))zz=o$zz
  if(!is.null(o$aa))aa=o$aa else aa=NULL
  if(!is.null(o$ll))ll=o$ll else ll=NULL
  
  
  #   browser()
  # hack TODO
  if(isblock) ylabb=""
  
#     browser()
  library(rapport) #for skewness
  xlabb=(str_wrap(xlabb,xlablen)) #set them now but will change later if necc
  ylabb=(str_wrap(ylabb,ylablen))
  #   p=otest(xx,yy)
  #   if(simple)browser()
  if(is.null(yy) ){
    q=if(pie) opie(xx,fillcolour,nfacetcol=nfacetcol,histlabs,sizefac,xbreakswrap=xbreakswrap,ybreakswrap=ybreakswrap) else zbar(xx,fillcolour,histlabs,sizefac,img=img,xbreakswrap=xbreakswrap,ybreakswrap=ybreakswrap)
    ylabb="Count"
  }  else {
    if(is.null(p))stop(paste0("p failed"),xx,yy)
    #     if(p[1]==0) warning(paste0("you got a zero significance: ",xlabb," or ",ylabb))

    
    if(simple) {
      if(classer(xx) %in% xc("con ord") & classer(yy) %in% xc("con ord")){
        q=simpleline(xx,yy)
      } 
      else q=simplebar(xx,yy,mypal=mypal)
      ylabb="Count"
    }
    
    else {
#             browser()
      if(classer(xx)!="con" & classer(yy)!="con"){
        if(percent)q=oplot_discrete_discrete_percent(xx,yy,zz=zz,aa=aa,ll=ll,sizefac=sizefac,img=img,position=position,xbreakswrap=xbreakswrap,ybreakswrap=ybreakswrap)    
        else if(mosaic)q=ggMMplot(xx,yy,zz=zz,aa=aa,ll=ll,sizefac=sizefac,excludeSmall.x=excludeSmall.x,excludeSmall.y=excludeSmall.y)     else q=oplot_discrete_discrete(xx,yy,zz=zz,aa=aa,ll=ll,sizefac=sizefac,xbreakswrap=xbreakswrap,ybreakswrap=ybreakswrap)
      }
      
      else if(classer(xx)!="con" & classer(yy)=="con"){
        q=oplot_discrete_continuous(xx,yy,zz=zz,...)
      } 
      
      else if(classer(xx)=="con" & classer(yy)!="con"){
        q=oplot_continuous_discrete(xx,yy,zz=zz,mypal=mypal,ylabb=ylabb)
        ylabb="Proportion"
        
      }  
      else q=oplot_continuous_continuous(xx,yy)
    }
  }
#   browser()
  if(ylabb=="NULL")ylabb=NULL
  if(is.null(ylabb))  q=q+ylab(ylabb) else q=q+ylab("") 
    if(percent | mosaic)   q=q+ylab("Percent of alumni")+scale_fill_brewer(palette="YlOrBr",guide= guide_legend(title = ylabb,reverse=T) )#this can't go inside discrete_discrete_percent because the label gets lost
    else 
      if(pie)   q=q+ylab("")+scale_x_discrete(breaks=NULL)+scale_fill_brewer(palette="YlOrBr",guide= guide_legend(title = ylabb) )#this can't go inside discrete_discrete_percent because the label gets lost


  q=q+
    #     xlab(waiver())+
    xlab(xlabb)+
    #     ggtitle(bquote(atop(.(tit), atop(italic(.(stat)), ""))))+
    if(printSig)ggtitle(attr(p,"phrase"))+
    theme(
      plot.title=element_text(size=rel(1.2))
      ,axis.text.y=element_text(size=rel(1.4),hjust=1)
      ,axis.title.y=element_text(size= rel(1.6),angle=0)
      ,axis.title.x=element_text(size=rel(1.6),angle=0)
      ,axis.text.x=element_text(angle=ifelse(classer(xx)=="con" | 20>nchar(paste(names(table(xx)),collapse="")),0,-90),hjust=0,size=if(pie)rel(.6) else rel(1.4))        
      
      
      
    )
  #TODO a hack cos we dont have full support for integers
  if(!is.null(attr(xx,"setlevout"))) if(attr(xx,"setlevout")=="int") q=q+
    scale_x_discrete(breaks=c(0:max(table(xx)))) 
  #TODO a hack cos we dont have full support for integers
  
  
  if(!is.null(p)) if(!is.na(p))   if(p<=sigLev | is.null(yy)) {

    attr(q,"note")=c(attr(q,"note"),msg)
    
#     revLab=function(lab)which(sapply(a13.r,attr,"label")==lab)
    
#     q=q+annotate(geom="text",x=min(xx,na.rm=T),y=0,hjust=0,vjust=-1,label=paste0(revLab(xlabb),"--",revLab(ylabb),xlabb,"--",ylabb))

    q
  } 
}



oplot_continuous_discrete=function(xx,yy,zz=NULL,mypal="YlOrBr",ylabb=""){
  #   browser()
  library(scales)
  #   ggplot(data=data.frame(xx,yy),aes(yy,xx))+geom_violin(scale="count")+ stat_sum_df("mean_cl_boot",colour="orange")#smean.cl.boot is a very fast implementation of the basic nonparametric bootstrap for obtaining confidence limits for the population mean without assuming normality. These functions all delete NAs automatically.
  #   ggplot(data=dd,aes(x=dd[,1],y=dd[,2]))+geom_jitter(size=4,alpha=.3,colour="green",position = position_jitter(width = .2,height=.2))#+ stat_sum_df("mean_cl_boot",colour="orange")#
  ggplot(data=data.frame(xx,yy),aes(x=xx,fill=yy))+geom_density(position="fill",colour="black")+
    scale_fill_brewer(guide = guide_legend(title=ylabb)
                      ,type="seq",palette=mypal,breaks=rev(levels(yy)))+
    scale_y_continuous(labels=percent)
  
}

oplot_discrete_continuous=function(xx,yy,zz=NULL,xbreakswrap=30){
  dd=data.frame(xx,yy)
  #   browser()
  q=ggplot(data=dd,aes(x=xx,y=yy))+
    geom_jitter(size=4,alpha=.3,colour="green",position = position_jitter(width = .01*length(table(xx)),height=.1*length(table(xx))))+
    geom_boxplot(alpha=.5, outlier.size = 3,outlier.colour = "red",colour="darkgreen",fill="orange",line_width=60)
  attr(q,"note")="The orange boxes contain the middle half of the points, and the line inside the box is the median. Red dots are outliers"
  
  q=q+scale_x_discrete(breaks=levels(factor(xx)),labels=str_wrap(levels(factor(xx)),xbreakswrap))
  q#   ggplot(data=dd,aes(x=xx,y=yy))+geom_boxplot(colour="red")+geom_jitter(size=4,alpha=.3,colour="green",position = position_jitter(width = .2,height=.2))
}

oplot_discrete_discrete=function(xx,yy,zz=NULL,aa=aa,ll=NULL,sizefac=11,img=NULL,ybreakswrap=30,xbreakswrap=30){
  #   cat(attr(xx,"label"),attr(yy,"label"))
  #   warning(attr(xx,"label"),attr(yy,"label"))
  #   browser()
  tmp2=data.frame(x2=xx,y2=yy,zz=zz,aa=aa,ll=ll,value=zz,large=ll)
  
  if( all(aa==1)) { #all sizes the same so it must be a ggheat
    q= ggplot(tmp2,aes(x=x2,y=y2,fill=zz,label=ll))+
      geom_tile() +
      geom_text(size=3,colour="black")
    attr(q,"note")="my ggheatnote."
    q=q+scale_fill_gradient2(low="blue",mid="grey",high="red",labels=xc("positive connection;neutral;negative connection",sep=";"),breaks=c(max(tmp2$zz),0,min(tmp2$zz)),guide= guide_legend(title = "Colour"))
    
  } else {
    q= ggplot(tmp2,aes(x=x2,y=y2,size=aa,fill=zz,label=ll))+
      geom_point(alpha=.5,shape=21,colour="black")+scale_shape(solid=FALSE)+
      geom_text(size=5,colour="black")+
      scale_size_area(max_size=4000/(sizefac*sizefac),guide="none")
    attr(q,"note")="Bigger numbers are written bigger. Surprisingly large numbers are more red and suprisingly small numbers are more blue."
    q=q+scale_fill_gradient2(low="blue",mid="grey",high="red",labels=xc("more than expected;neutral;less than expected",sep=";"),breaks=c(max(tmp2$zz),0,min(tmp2$zz)),guide= guide_legend(title = "Colour"))
    
  }
#   browser()
  q=q+scale_x_discrete(breaks=levels(factor(xx)),labels=str_wrap(levels(factor(xx)),xbreakswrap))
    q=q+scale_y_discrete(breaks=levels(factor(yy)),labels=str_wrap(levels(factor(yy)),ybreakswrap)) 
  q 
}


oplot_discrete_discrete_percent=function(xx,yy,zz=NULL,aa=aa,ll=NULL
                                         ,position="fill",sizefac=11,ybreakswrap=30,xbreakswrap=30,img=img){
  tmp2=data.frame(x2=xx,y2=yy,zz=zz,aa=aa,ll=ll,value=zz,large=ll)
  
#   browser() 
  if( all(aa==1)) { #all sizes the same so it must be a ggheat
    stop("this case not covered yet")
    
  } else {
    library(scales)
#     browser()
    tmp2=tmp2[order(match(tmp2$y2,levels(tmp2$y2))),] #have to do this because without it, for some reason the legend labels are correctly ordered but the colours in the chart though correct are in the order in the data. FIXME

q=ggplot(tmp2,aes(x=x2,y=aa,fill=y2)) 
if(!is.null(img)) {
  library(png)
  library(grid)
  img <- readPNG(img)
  g <- rasterGrob(img, interpolate=TRUE)
  
  q=q+annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=max(tmp2$y2))
} 
q=q+geom_bar(stat="identity",position=position)+
   scale_y_continuous( labels= percent )
   
    attr(q,"note")="comlpete me"
    
    
  }
  q=q+scale_x_discrete(breaks=levels(factor(tmp2$x2)),labels=str_wrap(levels(factor(tmp2$x2)),xbreakswrap))
  q
}



# thanks to Edwin: https://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2
ggMMplot <- function(xx,yy,zz=zz,aa=aa,ll=ll,sizefac=sizefac,excludeSmall.x=.1,excludeSmall.y=.1){
  require(ggplot2)
  
  df=data.frame(xx,yy)
#   browser()
  if(!is.null(excludeSmall.x) | !is.null(excludeSmall.y)){
  x.l=data.frame(table(xx)/nrow(df))
  df2=merge(df,x.l)
  y.l=data.frame(table(yy)/nrow(df))
  df2=merge(df2,y.l,by="yy")
  df3=df2[df2$Freq.x>excludeSmall.x & df2$Freq.y>excludeSmall.y,]
  xx=droplevels(df3$xx)
  yy=droplevels(df3$yy)
  }
  levxx <- length(levels(xx))
  levyy <- length(levels(yy))
  
  jointTable <- prop.table(table(xx, yy))
  ord=order(jointTable[,ncol(jointTable)])
  
#   jointTable=as.table(jointTable[rev(rownames(jointTable)),])
  
  plotData <- as.data.frame(jointTable)
  plotData$marginxx <- prop.table(table(xx))
  plotData$yyHeight <- plotData$Freq / plotData$marginxx
  plotData$xxCenter <- c(0, cumsum(plotData$marginxx)[1:levxx -1]) +
    plotData$marginxx / 2
#   browser()
library(scales)
  q=ggplot(plotData, aes(xxCenter, yyHeight)) +
    geom_bar(stat = "identity", aes(width = marginxx, fill = yy), col = "Black") +
    geom_text(aes(label = as.character(xx), x = xxCenter, y = 1.05),size=50/sizefac) +
    scale_size_area(guide=F)+scale_y_continuous(label=percent)+scale_x_continuous(labels=NULL)
if(nrow(df2)>nrow(df3))warning("Plot excludes very infrequent categories")
attr(q,"note")="Plot excludes very infrequent categories"
q
}



oplot_continuous_continuous=function(xx,yy){
  dd=data.frame(xx,yy)
  q=ggplot(data=dd,aes(x=xx,y=yy))+geom_jitter(size=4,alpha=.4,colour="green")+geom_smooth(colour="orange",size=1.5)#+geom_smooth(method="lm",colour="red",alpha=.1,size=1)
  attr(q,"note")="The red line is the best straight line through the points, and the orange line is the best curved line"
  q
}

zbar=function(xx,fillcolour=brewer.pal(3,"YlOrBr")[2],histlabs=T,img=NULL,sizefac=9,na.omit=T,xbreakswrap=xbreakswrap,ybreakswrap=ybreakswrap){
#   browser()
  if(na.omit)xx=na.omit(data.frame(xx))
  
  
  q=ggplot(data=data.frame(xx),aes(x=xx))
  
  if(!is.null(img)) {
    library(png)
    library(grid)
    img <- readPNG(img)
    g <- rasterGrob(img, interpolate=TRUE)
    
    q=q+annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  } 
 q=q  +geom_histogram(fill=fillcolour)+
    scale_y_continuous()+if(histlabs)stat_bin(aes(label=(..count..)), geom="text", vjust=1.4,position="identity",size=80/sizefac,colour="darkgreen") 
  attr(q,"nx")=length(unique(xx))
  attr(q,"note")="mynotekill"
  xx=xx$xx
  
  q=q+scale_x_discrete(breaks=levels(factor(xx)),labels=str_wrap(levels(factor(xx)),xbreakswrap))
#   q=q+scale_y_discrete(breaks=levels(factor(yy)),labels=str_wrap(levels(factor(yy)),ybreakswrap)) 
  
  q
}

opie=function(xx,fillcolour=brewer.pal(3,"YlOrBr")[2],histlabs=T,sizefac=9,na.omit=T,
              xbreakswrap=xbreakswrap,ybreakswrap=ybreakswrap,nfacetcol){
#   browser()
  facet=T
  if(na.omit)xx=na.omit(xx)
  xx=factor(xx,levels=levels(xx),ord=T)
  xx=str_wrap(xx,xbreakswrap)
  df=data.frame(xx)
q=ggplot(df,aes(x=factor(1),fill=xx))+
  geom_bar(width=1,alpha=.8,position="stack")+coord_polar(theta="y")+
  geom_rect(aes(xmin=0,ymin=0,xmax=.5,ymax=.8),stat="identity",fill="white")+
  
  scale_y_continuous(labels=xc("0% 25% 50% 75%"),breaks=nrow(df)*c(0,.25,.50,.75),limits=c(0,nrow(df)))+ 
  theme(axis.text.x=element_text(size=6),strip.text=element_text(size=100/sizefac),strip.background = element_rect(fill="white",colour="white"),panel.border = element_rect(colour="white"))
#         ,panel.border=element_rect(border.width=0))

if(facet)q=q+facet_wrap(ncol = nfacetcol,~xx)+scale_fill_continuous(guide=F)
  attr(q,"note")="mynotekill"
  
q=q+scale_x_discrete(breaks=NULL)

  q 
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
  q=ggplot(data.frame(xx,yy),aes(x=xx,fill=yy))+geom_bar(colour="black",position="fill")+
    scale_fill_brewer(guide = guide_legend(title=attr(yy,"label")),type="seq",palette=mypal,breaks=rev(levels(yy)))
  attr(q,"note")=""
  q
}

#' Just a wrapper for rworldmap
#'
#' @param data with xx= country names and yy value to be plotted
#' @param title title
#' @export
#' @family main omnivr functions
#' @example R/examples/ex-worldmap.R
#' @return A plot
worldmap=function(data,cutData=T,breaks=NULL,labels=NULL,legendLabel="Category",mapTitle="",xlim=NULL,ylim=NULL,...){
  library(rworldmap)
  library(rgdal) # note you also need sudo apt-get install librproj-dev and libgdal1-dev
    #first get countries excluding Antarctica which crashes spTransform
#     sPDF <- getMap()[-which(getMap()$ADMIN=="Antarctica"),]
      #transform to robin for the Robinson projection
#   browser()
  if(cutData)data$yy=cut(as.numeric(as.character(data$yy)),breaks=breaks,labels=labels)
data[,legendLabel]=data$yy
data[,"Number"]=data$ll
colourPalette=brewer.pal(length(unique(data$yy)),"RdPu")
  sPDF <- joinCountryData2Map( data, joinCode = "NAME", nameJoinColumn = "xx",verbose = F )
    sPDF <- spTransform(sPDF, CRS=CRS("+proj=eck6 +lon_0=0"))#Eckert VI equal-area projection
bb=sPDF@bbox
if(is.null(xlim))xlim=(sPDF@bbox[1,])
if(is.null(ylim))ylim=(sPDF@bbox[2,])
  par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")

if("ll" %in% colnames(data)){
  mapBubbles(sPDF,symbolSize = 2,nameZSize="Number",nameZColour = legendLabel ,colourPalette=colourPalette
             ,xlim=xlim,ylim=ylim,mapTitle=mapTitle,...)
} else {
#   browser()
  mapCountryData( sPDF, nameColumnToPlot=legendLabel ,colourPalette=colourPalette
                  ,xlim=xlim,ylim=(sPDF@bbox[2,]),mapTitle=mapTitle,...)
}
}

oqgraph=function(xx,yy,zz){
  df=cast(otrans(data.frame(xx,yy,zz)),xx~yy,value = "zz")
  library(qgraph)
#   library(png)
labs=str_wrap(findlabs(colnames(df),df),20)
#   groups=factor(  c(rep("now",ncol(now)),rep("back",ncol(back))))
#   pix=(  c(rep("pix/run.png",ncol(now)),rep("pix/lupa.png",ncol(back))))
  if(class(try(
    qgraph(df,minimum=.1, layout="spring",label.cex=0.9,  vsize=3,color=xc("gray lightgreen"),borders=F,label.scale=FALSE,details=T,title="",labels=labs    )
  ))!="try-error") 
    qgraph(df,minimum=.1, layout="spring",label.cex=0.9,  vsize=3,color=xc("gray lightgreen"),borders=F,label.scale=FALSE,details=T,title="",labels=labs    )
}


facet_labeller=function(var,value){
  value=str_wrap(value,8)
}

labb=function(x)ifelse(is.null(attr(x,"label")),deparse(substitute(x)),attr(x,"label"))


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
sankplot=function(mm,minedge=1,strw=12,pastel=T){
  library(riverplot)
  library(stringr)
  
  ## new version
  mo=na.omit(mm)
  edges=mo[,c(2,1,3)]
  edges[,1]=paste(edges[,1],"1",sep="")
  edges[,2]=paste(edges[,2],"2",sep="")
  colnames(edges)=c(paste0("N",1:2),"Value")
  
  
  ## same

  edges=edges[edges$Value>minedge,]
  
  nodes=data.frame(rbind(
    cbind(paste(unique(edges$N1)),1),
    cbind(paste(unique(edges$N2)),2) #have to differentiate because the package will complain if nodes in 2 cols have same name
  ),stringsAsFactors=F)
  
  
  nodes$X2=as.numeric(nodes$X2)
  
  nodes$labels=sapply(nodes[,1],str_sub,1,-2)
  
#   browser()
  labels=(unique(nodes$labels))
  if(pastel)col=colorRampPaletteAlpha(c( "#F1A34066", "#998EC366" ))(length(labels))
# col=colorRampPaletteAlpha(c( "green", "yellow" ))(length(labels))
# col=rep("",length(labels))
#   col=rev(terrain.colors(length(labels)))
  concol=data.frame(labels,col,stringsAsFactors=F)
  
# browser()
  
  nodesc=join(nodes,concol,by="labels")
  colnames(nodesc)[1:2]=c("ID","x")

# c(levels(mm$xx),levels(mm$yy))
# c(rep(1,length(levels(mm$xx))),rep(2,length(levels(mm$yy))))

  # nodesc$col[nodesc$x==2]=NA
# nodesc$ypos=""
# fil1=nodesc$x==1
# nodesc$ypos[fil1]=
nodesc$labels=str_wrap(nodesc$labels,strw)


  makeRiver(nodesc,edges)
}


psankplot=function(s,t1="Before AIMS",t2="Now",node_margin=.2,nodewidth=5){
#   browser()
  plot(s,srt=0,nodewidth=nodewidth)
  text(1,50,str_wrap(t1,32),cex=1,col = "red")
  text(2,50,str_wrap(t2,32),cex=1,col="red")
  box("figure")
  
#   graphics.off()
}

## just overwriting the original which uses cat for its warnings
joinCountryData2Map=function (dF, joinCode = "ISO3", nameJoinColumn = "ISO3V10", 
          nameCountryColumn = "Country", suggestForFailedCodes = FALSE, 
          mapResolution = "coarse", projection = NA, verbose = FALSE) 
{
  mapWithData <- getMap(resolution = mapResolution)
  if (!is.na(projection)) 
    warning("the projection argument has been deprecated, returning Lat Lon, use spTransform from package rgdal as shown in help details or the FAQ")
  listJoinCodesNew <- c("ISO_A2", "ISO_A3", "FIPS_10_", "ADMIN", 
                        "ISO_N3")
  listJoinCodesOld <- c("ISO2", "ISO3", "FIPS", "NAME", "UN")
  listJoinCodes <- c(listJoinCodesOld, listJoinCodesNew)
  if (joinCode %in% listJoinCodes == FALSE) {
    stop("your joinCode (", joinCode, ") in joinCountryData2Map() is not one of those supported. Options are :", 
         paste(listJoinCodes, ""), "\n")
    return(FALSE)
  }
  joinCodeOld <- joinCode
  if (joinCode %in% listJoinCodesOld) {
    joinCode <- listJoinCodesNew[match(joinCode, listJoinCodesOld)]
  }
  if (is.na(match(nameJoinColumn, names(dF)))) {
    stop("your chosen nameJoinColumn :'", nameJoinColumn, 
         "' seems not to exist in your data, columns = ", 
         paste(names(dF), ""))
    return(FALSE)
  }
  dF[[joinCode]] <- as.character(dF[[nameJoinColumn]])
  dF[[joinCode]] <- gsub("[[:space:]]*$", "", dF[[joinCode]])
  if (joinCode == "ADMIN") {
    dF$ISO3 <- NA
    for (i in 1:nrow(dF)) dF$ISO3[i] = rwmGetISO3(dF[[joinCode]][i])
    joinCode = "ISO3"
    nameCountryColumn = nameJoinColumn
  }
  matchPosnsInLookup <- match(as.character(dF[[joinCode]]), 
                              as.character(mapWithData@data[[joinCode]]))
  failedCodes <- dF[[joinCode]][is.na(matchPosnsInLookup)]
  numFailedCodes <- length(failedCodes)
  numMatchedCountries <- nrow(dF) - numFailedCodes
  warning(numMatchedCountries, "codes from your data successfully matched countries in the map\n")
  failedCountries <- dF[[nameCountryColumn]][is.na(matchPosnsInLookup)]
  failedCountries <- cbind(failedCodes, failedCountries = as.character(failedCountries))
  warning(numFailedCodes, "codes from your data failed to match with a country code in the map\n")
  if (verbose) 
    print(failedCountries)
  matchPosnsInUserData <- match(as.character(mapWithData@data[[joinCode]]), 
                                as.character(dF[[joinCode]]))
  codesMissingFromUserData <- as.character(mapWithData@data[[joinCode]][is.na(matchPosnsInUserData)])
  countriesMissingFromUserData <- as.character(mapWithData@data[["NAME"]][is.na(matchPosnsInUserData)])
  numMissingCodes <- length(codesMissingFromUserData)
  warning(numMissingCodes, "codes from the map weren't represented in your data\n")
  mapWithData@data <- cbind(mapWithData@data, dF[matchPosnsInUserData, 
                                                 ])
  invisible(mapWithData)
}



# ggMMplot(diamonds$cut, diamonds$clarity)