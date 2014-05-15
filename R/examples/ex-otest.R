con=mtcars$wt
ord=factor(mtcars$gear,ordered=T)
nom=factor(mtcars$cyl,ordered=F)

otest(nom,ord)
#same as:
otest_nom_ord(nom,ord)

#users can provide their own functions to overwrite functions provided by the package.
otest_nom_ord=function(xx,yy,spv)
{      
  pp=kruskal.test(factor(xx,ordered=T),yy)
  p=pp$p.value
  attr(p,"estimate")=pp$statistic
  attr(p,"method")="The user has changed just this text as an exercise"
  p
}
otest_nom_ord(nom,ord)


#users can define their own data types and provide corresponding functions.
attr(ord,"setlevout")="myNewDataType"
otest_nom_myNewDataType=function(xx,yy,spv)
{      
  pp=kruskal.test(factor(xx,ordered=T),yy)
  p=pp$p.value
  attr(p,"estimate")=pp$statistic
  attr(p,"method")="The user has changed just this text once again as an exercise"
  p
}
otest_nom_myNewDataType(nom,ord)
