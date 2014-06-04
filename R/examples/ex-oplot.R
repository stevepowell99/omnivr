library(omnivr)
library(dplyr)
library(reshape)
library(stringr)
library(ggplot2)


annfunc=function(text)annotate(geom="text",x=0,y=0,hjust=0,vjust=-1,label=text)

mtcars$gear=factor(mtcars$gear,ord=T)
mtcars$cyl=factor(mtcars$cyl,ord=T)
attr(mtcars$gear,"label")="My gear label"
attr(mtcars$disp,"label")="The displacement"
attr(mtcars$cyl,"label")="Number of cylinders"

oplot(mtcars$cyl)+annfunc("A factor")
oplot(mtcars$cyl,pie=T)
# note this makes sense when each level is a percentage

oplot(mtcars$cyl,img="result.png")+annfunc("A factor with a background")
oplot(mtcars$disp)+annfunc("A continuous variable")
oplot(mtcars$wt)+annfunc("This one had no label")
oplot(mtcars$disp,histlabs = F)+annfunc("No histogram count labels")

oplot(mtcars$disp,mtcars$cyl)+annfunc("Continuous against factor")
oplot(mtcars$wt,mtcars$disp)+annfunc("Two continuous variables")
oplot(mtcars$wt,mtcars$disp)+annfunc("You can add what you want")+stat_binhex(bins = 9)

oplot(mtcars$gear,mtcars$cyl,sigLev=1)+annfunc("Two factors")
oplot(mtcars$gear,mtcars$cyl,sigLev=1,excludeSmall.x=1)+annfunc("Two factors")
oplot(mtcars$gear,mtcars$cyl,sigLev=1,reorder.y=T)+annfunc("Two factors with reordering")
oplot(mtcars$gear,mtcars$cyl,sigLev=1,percent=T)+annfunc("Two factors with 100% bar")
oplot(mtcars$gear,mtcars$cyl,sigLev=1,mosaic=T)+annfunc("Two factors - mosaic")

oplot(mtcars$wt,mtcars$disp,simple = T)+annfunc("Simple version")

#some continuous data
  a=rnorm(9,1);attr(a,"label")="alabel"
  b=rnorm(9,2);attr(b,"label")="blabel"
  c=rnorm(9,3);attr(c,"label")="clabel"
  d=rnorm(9,4);attr(d,"label")="dlabel"
con=data.frame(a,b,c,d
  )

oplot(con,sigLev=1)+annfunc("Block of continuous variables")

conf=data.frame(sapply(con,function(x)factor(ifelse(x>2,"yes","no"))))
oplot(conf,sigLev=1)+annfunc("Block of factors")
ggplot(data=otrans(conf),aes(xx,yy,size=ll,colour=zz))+geom_point()

ggplot(data=otrans(conf,xfilter="yes"),aes(xx))+geom_bar(fill="orange")+annfunc("Multichoice with filter")



oplot(conf,sigLev=1,xfilter="yes")

other=rnorm(9,nrow(conf))
oplot(conf,other,sigLev=1,xfilter="yes")+annfunc("Multi-choice block against continuous variable")
otherf=factor(rnorm(9,nrow(conf))>8.4,labels=c("black","white"))
oplot(conf,otherf,sigLev=1,xfilter="yes")+annfunc("Multi-choice block against single factor")

## lists

likefish=c(1,1,2,2,3,3)
likebeer=c(1,1,1,1,1,2)
ff=data.frame(likefish,likebeer)
gg=data.frame(likefish,likebeer)[1:4,]
attr(ff,"datafilename")="the f one"
attr(gg,"datafilename")="the g one"
xxx=list(ff,gg)

oplot(xxx,sigLev=1)
oplot(xxx,sigLev=1,percent=T)

## table =T
### with real tables
oplot(table(mtcars$cyl),table=T,sigLev=1)
oplot(table(mtcars$cyl,mtcars$gear),table=T,sigLev=1)

### with fake tables
tab=mtcars[1:3,1:3]
oplot(tab,table=T,sigLev=1)

## it would be great if the list functionality called the first part of oplot. e.g. so you could feed it a list of tables

### sankplot
plot(sankplot(otrans(mtcars$cyl,mtcars$gear)),srt=0,nodewidth=5)
