library(omnivr)
library(dplyr)
library(reshape)
library(stringr)
library(ggplot2)



mtcars$gear=factor(mtcars$gear,ord=T)
mtcars$cyl=factor(mtcars$cyl,ord=T)
attr(mtcars$gear,"label")="My gear label"
attr(mtcars$disp,"label")="The displacement"
attr(mtcars$cyl,"label")="Number of cylinders"

oplot(mtcars$cyl)+ggtitle("A factor")
oplot(mtcars$disp)+ggtitle("A continuous variable")
oplot(mtcars$wt)+ggtitle("This one had no label")
oplot(mtcars$disp,histlabs = F)+ggtitle("No histogram count labels")

oplot(mtcars$disp,mtcars$cyl)+ggtitle("Continuous against factor")
oplot(mtcars$cyl,mtcars$disp)+ggtitle("Factor against continuous")
oplot(mtcars$wt,mtcars$disp)+ggtitle("Two continuous variables")
oplot(mtcars$gear,mtcars$cyl,sigLev=2)+ggtitle("Two factors")

#some continuous data
  a=rnorm(9,1);attr(a,"label")="alabel"
  b=rnorm(9,1);attr(b,"label")="blabel"
  c=rnorm(9,1);attr(c,"label")="clabel"
  d=rnorm(9,1);attr(d,"label")="dlabel"
con=data.frame(a,b,c,d
  )

oplot(con,sigLev=1)
conf=data.frame(sapply(con,function(x)factor(ifelse(x>0,"yes","no"))))
oplot(conf,sigLev=1)



oplot(conf,sigLev=1,xfilter="yes")

## lists

likefish=c(1,1,2,2,3,3)
likebeer=c(1,1,1,1,1,2)
ff=data.frame(likefish,likebeer)
gg=data.frame(likefish,likebeer)[1:4,]
attr(ff,"datafilename")="the f one"
attr(gg,"datafilename")="the g one"
xxx=list(ff,gg)

oplot(xxx,sigLev=1)
