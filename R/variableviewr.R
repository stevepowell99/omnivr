#' Returns a block of variables based on their attributes
#' 
#' Note the dataset it returns also has a label attribute which tries to extract an appropriate label
#' from the attribute
#' @param dat  dataset
#' @param att the attribute
#' @param equ the value of the attribute. If equ is "list", then findr will return a list.
#' @param datafilename useful if you are working with different data files. wprop applies this. Used by otrans and oprop when submitting a list of variables or blocks
#' @family experimental
#' @return A dataset
#' 
#' @examples Here are some examples
findr=function(att="block",dat=data.r,equ="x",datafilename="",...){

    if(equ=="list") {
      l=unique(sapply(dat,attr,att))
      l=na.omit(l)
#       browser()
      d=lapply(l,function(x) findr_inner(att=att,dat=dat,equ=x,datafilename=datafilename,...))
      names(d)=l

    } else d=findr_inner(att,dat,equ,datafilename=datafilename,...)  

 d
}

findr_inner=function(att="block",dat=data.r,equ="x",datafilename=""){
  
#       browser()
    x=equ==sapply(dat,attr,att)
    x[is.na(x)]=F
#         browser()
    d=data.frame(dat[,x])
   
  
  if(equ=="x") eqx=NULL else eqx=equ
  attr(d,"label")=paste(att,eqx,sep=":")
  attr(d,"datafilename")=datafilename
  attr(d,"blocktitle")=unique(na.omit(sapply(d,attr,"blocktitle"))) #could put in a warning if there are tow
  
  
  if(nrow(d)==0 | ncol(d)==0) warning(cat("findr returned an empty block: ",att,equ))
  if(datafilename!="") d=data.frame(lapply(d,function(x){
# browser()
    attr(x,"datafilename")=datafilename
    x
  }))
  d
}




thisfindr=function(att,equ="x",dat=l,wstemp=ws){
  #   
  if(!(att %in% colnames(wstemp)))stop(P("There is something wrong with the attribute name for the thisfindr function: ",att))
  wss=wstemp[,att]==equ
  wss[is.na(wss)]=FALSE
  data.frame(dat)[,wss] #has to be -1 because l still has a leading stupid empty vector which is cut off at the end
}

oprop1=function(df,labnames=xc("varnames newvarnames label shortlabs ncol formula recode setlevout nt n1 n2 n3")){ #gets stuff from the data only
  #   df=get(nam,envir=.GlobalEnv)
  #   df<<-df
  colnames(df)=make.names(colnames(df),unique=T)
  sapply(colnames(df),function(xx){
    x=df[,xx]
    t=table(x)
    tt=names(t[rev(order(t))])
    sapply(labnames,function(y){
      
      if(y=="varnames") xx else {
        if(y=="ncol") min(which(xx==colnames(df))) else {
          if(y=="label") ifelse(is.null(attr(x,"label")),xx,attr(x,"label")) else {
            if(y=="shortlabs") ifelse(is.null(attr(x,"label")),xx,attr(x,"label")) else {
              if(y=="setlev") classer(x) else {
                if(y=="setlevout") classer(x) else {
                  if(y=="nt") length(unique(x)) else {
                    if(y=="n1") paste(tt[1:min(10,length(tt))],collapse=";;") else {
                      if(y=="n2") "" else {
                        if(y=="n3") "" else {
                          a=attr(x,y)
                          if(is.null(a)) '' else a
                        }}}}}}}}}}
    })
  })
  
}#it gets the possibly properties


variableviewr=function(nam="data",type="",winner="",labnames=NULL,method="quicker",set_doclusters=F,onlyLoad=F){
  
  if(onlyLoad){
    data.r=readRDS("data.r")
    blocks=readRDS("blocks")
  } else {
    if(is.null(labnames))labnames=xc("varnames newvarnames label shortlabs ncol formula recode setlevout nt n1 n2 n3")
    nnn=paste0("prop.",nam,".csv")
    nn=paste0("variableviewr/",nnn)
    tmp=get(nam,envir=.GlobalEnv)
    colnames(tmp)=make.names(colnames(tmp),unique=T)# have to do this because the xlconnect and csv calls wil do it
    assign(nam,tmp,envir=.GlobalEnv)##reassign the new tidy colnames
    if(!file.exists(nn)) {
      l=tmp
      winner="new"
      #     oo=t(data.frame(oprop1(l)))
      #     newWS=ws=data.frame(oo)
    } else {
      winner="file"
      ws <- read.csv(nn)
      sheets <<- dir("variableviewr")#getSheets(wb)
      sheets=sheets[!(sheets %in% nnn)]
      #      
      #read in worksheet and create list l to receive the this's from tmp
      rownames(ws)=make.names(ws[,1],unique=T)
      colnames(ws)[1]="varnames"
      
      ws<<-ws
      warning("reading in file")
      #      l=list(repaste0(times=nrow(tmp),99))
      l=as.list(rep(times=nrow(ws),99))
      names(l)=ws$varnames
      
      
      
      for (xx in 1:nrow(ws)){   
        x=rownames(ws)[xx]
        warning(x)
        if(x %in% colnames(tmp)) this=tmp[,x] else this=repaste0(NA,nrow(tmp))
        
#         this=as.character(this)
        ################### ooh are you sure
        
        
        #           if(x=="gear")
        if(!is.null(ws[x,"formula"])) {
          #           
          this=with(l,eval(parse(text=ws[x,"formula"])))
        } 
        new=!(x %in% rownames(ws))
        if(new) warning(paste0("Adding new rows to props file: ",x))
        for(y in colnames(ws)){
          
          
          #         if(y=="setlev"){
          #           if(is.null(ws[x,y]))next()
          #           if(ws[x,y]=="str")this=as.character(this)
          #           if(ws[x,y]=="nom")this=factor(this,ordered=F)
          #           if(ws[x,y]=="ord"){this=factor(this,ordered=T);warning(paste0("app",ws[x,y]))}
          #           if(ws[x,y]=="int")this=as.integer(this)
          #           if(ws[x,y]=="con")this=as.numeric(this)
          #         }
          
          
          if(y=="recode" & !is.null(ws[x,y]) & !is.na(ws[x,y])){
              browser()
            #           if(x=="Yes")
            tabname=ws[x,y]
            ws$recode[is.na(ws$recode)]=""
            myblock<<-ws[ws$recode==tabname,]
            
            issingle=dim(myblock)[1]==1
            myvars=rownames(ws)[ws$recode==tabname]
            if(issingle) concord=data.frame("output"=names(table(this)),"input"=names(table(this))) else {
              mydata=tmp[,myvars]
              #             browser()
              blockvalues=do.call(what="c",as.list(mydata))
              mytable=table(blockvalues)
              concord=data.frame("output"=names(mytable),"input"=names(mytable))
              #             mytable=as.character(unique(unlist(lapply(mydata,unique))))
              #             concord=data.frame("output"=(mytable),"input"=(mytable))
            }
            if(tabname %in% sheets) {  #the worksheet already exists, only need to add any new data it found
              mySheet= read.csv(paste0("variableviewr/",tabname,".csv"))
              #             myrecodes=data.frame(mySheet[,1:ncol(mySheet)])
              
              concord=mySheet # temporarily switched off adding new data
              #             if(ws[x,"recode"]=="ones")
              # if data is to be con or int but has blanks, these shd be NA. str or factor cd be NA or blank, cater for both. xlconnect imports blank as NA so have to code NA into blank for non-numeric and "NA" into NA
              mySheet[is.na(mySheet)]=""
              mySheet[mySheet=="NA"]=NA #amusing switch because xlconnect imports empty string as NA
              if(ws[x,"setlevout"] %in% xc("con int")){
                this[this==""]=NA
              } #
              
              
              ins=mySheet[,2:ncol(mySheet)]
              outs=mySheet[,1]
              #             
              if(method=="slow" ){
                #                           browser()
                this=sapply(this,function(v){
                  instemp=ifelse(v=="" & !is.null(dim(ins)),ins[,1],ins)# ugly but otherwise when you have multiple input specs the trailing blanks get counted. This means that if you want a blank in the input spec it has to be in first column.
                  if(sum(instemp==v,na.rm=T)>1) stopaste0(paste0("you have this twice in your input specification: ",v)) 
                  if(!is.na(v)) whichrow=which(rowSums(data.frame(ins)==v,na.rm=T)>0) else whichrow=which(rowSums(is.na(data.frame(ins)),na.rm=T)>0) #because if v is NA we have to treat it specially
                  o=outs[whichrow]
                  ifelse(length(o)==0,v,o)
                  #             browser()
                })} else {
                  for(v in unlist(ins)){
                    if(!is.na(v)) whichrow=which(rowSums(data.frame(ins)==v,na.rm=T)>0) else whichrow=which(rowSums(is.na(data.frame(ins)),na.rm=T)>0) #because if v is NA we have to treat it specially
                    o=outs[min(whichrow)]
                    #               warning(paste(ins))
                    #               warning(paste(outs))
                    if(!is.na(v)){
                      if(length(o)!=0 )  if(!is.na(v)) {this[this==v]=o} 
                    } else this[is.na(this)]=o
                  } 
                }
              
              
              if(ws[x,"setlevout"] %in% xc("ord nom")){
                #               browser()
                this=factor(this,ordered=ifelse(ws[x,"setlevout"]=="ord",T,F),levels=(mySheet[,1]))
              }
            }
            #           
            if(myblock[1,1]==x){
              write.csv(file=paste0("variableviewr/",tabname,".csv"),concord)
#(nn,concord,tabname,styleAction=XLC$"STYLE_ACTION.NONE")##fixme so only does itonce for multiples
            }
            #           assign(tabname,myvars,envir=.GlobalEnv)#if it is the last one in the block, ensure that mydata[,tabname] gets this block
          }
          if(y=="setlevout"){
            if(is.null(ws[x,y]))next()
            if(ws[x,y]=="str")this=as.character(this)
            if(ws[x,y]=="nom")this=(factor(this,ordered=F)) #not sure what happens if there was already a recode above
            #           if(ws[x,y]=="ord"){this=droplevels(factor(this,ordered=T));warning(paste0("app",ws[x,y]))} #not sure why this is switched off
            if(ws[x,y]=="int")this=as.integer(this)
            if(ws[x,y]=="con")this=as.numeric(this)
          }
        }
        
        if(length(this)!=nrow(tmp)) {warning(paste0("Looks like you have a data frame within your data frame?",x));}
        #    
        
        l[[x]]=this;l<<-l
        names(l)[ncol(l)]=ws[x,"varnames"]
      } 
      #     l=l[,-1];
      l<<-l
# browser()
      l=data.frame(l)
      #     colnames(l)=rownames(ws)
      rownames(l)=rownames(tmp)
      orphans=data.frame(tmp[,!(colnames(tmp)  %in% colnames(l))])
      colnames(orphans)=colnames(tmp)[which(!(colnames(tmp)  %in% colnames(l)))]
      l=data.frame(l,orphans) # to add in any new rows in data tmp which were not in ws
      #     assign(nam,l,envir=.GlobalEnv)#dont think this is necessary
    }  
    #   warning(sapply(l,attributes))
    warning("rewriting props file from R object")
    # xx<<-l$Spol
    # yy<<-data$Spol
    # stopaste0()
    if(winner!="new"){  
      for(j in 1:ncol(l)){
        i=colnames(l)[j]
        store=attributes(l[,i])
        attributes(l[,i])=ws[i,]
        attr(l[,i],"ncol")=j
        attr(l[,i],"varnames")=i #to make sure the varnames are always correct
        ws[i,"varnames"]=i
        ws[i,"ncol"]=j
        # if(j==10)#       warning(ws[i,"ncol"])
        if(attr(l[,i],"setlevout")%in% xc("ord nom")) attr(l[,i],"levels")=store$levels
        if(attr(l[,i],"setlevout")%in% xc("ord nom")) attr(l[,i],"class")=store$class
      } 
      
      newWS=data.frame(ws)
      kill<<-newWS
    } else {
      
      oo=t(data.frame(oprop1(l)))
      newWS=ws=data.frame(oo)
    }  
    
    
    lold=l
    blocks=setdiff(colnames(ws),labnames)
    nblocks=blocks[str_sub(blocks,1,1)!="."]
    attr(l,"blocks")=blocks
    #   warning(winner)
    
    
    if(set_doclusters){  for(block in attr(l,"blocks")){
      #   
      l=makeClus(l,att=block,match="x")
      
    }}
    #   browser()
    attr(l,"blocks")=blocks
    blocks<<-blocks #fixme hack needed for xpairs steve
    
    # stopaste0()
    if(winner!="new"){ 
      for(j in 1:ncol(l)){
        i=make.names(colnames(l))[j]
        if(!(i %in% colnames(lold) )){
          #         
          attr(l[,i],"ncol")=j
          attr(l[,i],"varnames")=i #to make sure the varnames are always correct
          
          ws[i,"varnames"]=ws[i,"label"]=ws[i,"shortlabs"]=i
          ws[i,"ncol"]=j
          ws[i,"setlevout"]=attr(l[,i],"setlevout")
          
          
        } #it must have come from cluster addition and wasn't already covered
      } 
      colnames(l)=ifelse(is.na(ws[,"newvarnames"]),colnames(l),ws[,"newvarnames"])
      
      newWS=data.frame(ws)
      kill<<-newWS
    } 
    #   stopaste0(paste(colnames(newWS)))
    write.csv(file=nn,newWS)#writeWorksheetToFile(nn,newWS,"props",styleAction=XLC$"STYLE_ACTION.NONE")
    system(paste0("wmctrl -c ",nn," - LibreOffice Calc"), wait=FALSE)
    
    browseURL(paste0(nn),browser="/usr/bin/libreoffice")  
    
    for(j in 1:ncol(l)){
      #     
      i=colnames(l)[j]
      al=attributes(l[,i])
      waln=((names(al) %in% nblocks)) 
      walnn=al[waln]
      walnnn=walnn!="NA"
      attr(l[,i],"anyblocks")=any(walnnn)
      #       browser()
      warning(i)
#       if(is.null(classer(l[,i])))     stopaste0("something wrong with classer function")
      if(classer(l[,i])=="geo") {
        if(!all(xc("lat lon") %in% colnames(l))) stopaste0(paste0("You asked for a geographical display for variable ",i," but you didn't provide variables lat and/or lon"))
        
        attr(l[,i],"lat")=unlist(l[,"lat"])
        attr(l[,i],"lon")=unlist(l[,"lon"])
      }
    }
    
    
    assign(paste0(nam,".r"),l,envir=.GlobalEnv)# update tmp adding new cols if necessary
    #   stopaste0("assinged data")
    saveRDS(l,paste0(nam,".r"))# update tmp adding new cols if necessary
    
    saveRDS(blocks,"blocks")
  }    
  
}


wprop=function(nam="data",ftype="xl",type="",winner="",labnames,method="quicker",assigndata.r=T,set_doclusters=F,onlyLoad=F){
  
  if(onlyLoad){
#     data.r=readRDS(paste0(nam,".r"))
assign(paste0(nam,".r"),readRDS(paste0(nam,".r")),envir=.GlobalEnv)##reassign the new tidy colnames
#     saveRDS(l,paste0(nam,".r"))# update tmp adding new cols if necessary
if(file.exists(paste0(nam,".b")))assign(paste0(nam,".b"),readRDS(paste0(nam,".b")),envir=.GlobalEnv)#b=readRDS(paste0(nam,".b"))# 
if(file.exists(paste0(nam,".s")))assign(paste0(nam,".s"),readRDS(paste0(nam,".s")),envir=.GlobalEnv)#s=readRDS(paste0(nam,".s"))# 


#     specials=readRDS("specials")
  } else {
    if(xmb(labnames))labnames=xc("varnames newvarnames label shortlabs ncol formula recode setlevout nt n1 n2 n3")
    nn=paste0("prop.",nam,".xls")
    tmp=get(nam,envir=.GlobalEnv)
    colnames(tmp)=make.names(colnames(tmp),unique=T)# have to do this because the xlconnect and csv calls wil do it
    assign(nam,tmp,envir=.GlobalEnv)##reassign the new tidy colnames
    if(!file.exists(nn)) {
      l=tmp
      winner="new"
      #     oo=t(data.frame(oprop1(l)))
      #     newWS=ws=data.frame(oo)
    } else {
      winner="file"
      wb <- loadWorkbook(nn)
      sheets <<- getSheets(wb)
      #      
      #read in worksheet and create list l to receive the this's from tmp
      ws= readWorksheetFromFile(nn,sheet="props",rownames=NULL,colTypes="character",startCol=1)
      rownames(ws)=make.names(ws[,1],unique=T)
      colnames(ws)[1]="varnames"
      
      ws<<-ws
      warning("reading in file")
      #      l=list(rep(times=nrow(tmp),99))
      l=as.list(rep(times=nrow(ws),99))
      names(l)=ws$varnames
      
      
      
      for (xx in 1:nrow(ws)){   
        x=rownames(ws)[xx]
        warning(x)
        if(x %in% colnames(tmp)) this=tmp[,x] else this=rep(NA,nrow(tmp))
        
        
        #           if(x=="gear")
        if(!xmb(ws[x,"formula"])) {
          #           
          this=with(l,eval(parse(text=ws[x,"formula"])))
        } 
        new=!(x %in% rownames(ws))
        if(new) warning(paste0("Adding new rows to props file: ",x))
        for(y in colnames(ws)){
          
          
          #         if(y=="setlev"){
          #           if(xmb(ws[x,y]))next()
          #           if(ws[x,y]=="str")this=as.character(this)
          #           if(ws[x,y]=="nom")this=factor(this,ordered=F)
          #           if(ws[x,y]=="ord"){this=factor(this,ordered=T);warning(paste0("app",ws[x,y]))}
          #           if(ws[x,y]=="int")this=as.integer(this)
          #           if(ws[x,y]=="con")this=as.numeric(this)
          #         }
          
          
          if(y=="recode" & !xmb(ws[x,y]) ){
            #           if(x=="Yes")
            tabname=ws[x,y]
            ws$recode[is.na(ws$recode)]=""
            myblock<<-ws[ws$recode==tabname,]
            
            issingle=dim(myblock)[1]==1
            myvars=rownames(ws)[ws$recode==tabname]
            if(issingle) concord=data.frame("output"=names(table(this)),"input"=names(table(this))) else {
              mydata=tmp[,myvars]
              #             browser()
              blockvalues=do.call(what="c",as.list(mydata))
              mytable=table(blockvalues)
              concord=data.frame("output"=names(mytable),"input"=names(mytable))
              #             mytable=as.character(unique(unlist(lapply(mydata,unique))))
              #             concord=data.frame("output"=(mytable),"input"=(mytable))
            }
            if(tabname %in% sheets) {  #the worksheet already exists, only need to add any new data it found
              mySheet= readWorksheetFromFile(nn,sheet=tabname,startCol=1,startRow=1,rownames=NULL,colTypes="character")#fixme if it is empty it will crash
              #             myrecodes=data.frame(mySheet[,1:ncol(mySheet)])
              
              concord=mySheet # temporarily switched off adding new data
              #             if(ws[x,"recode"]=="ones")
              # if data is to be con or int but has blanks, these shd be NA. str or factor cd be NA or blank, cater for both. xlconnect imports blank as NA so have to code NA into blank for non-numeric and "NA" into NA
              mySheet[is.na(mySheet)]=""
              mySheet[mySheet=="NA"]=NA #amusing switch because xlconnect imports empty string as NA
              if(ws[x,"setlevout"] %in% xc("con int")){
                this[this==""]=NA
              } #
              
              
              ins=mySheet[,2:ncol(mySheet)]
              outs=mySheet[,1]
              #             
              if(method=="slow" ){
                #                           browser()
                this=sapply(this,function(v){
                  instemp=ifelse(v=="" & !is.null(dim(ins)),ins[,1],ins)# ugly but otherwise when you have multiple input specs the trailing blanks get counted. This means that if you want a blank in the input spec it has to be in first column.
                  if(sum(instemp==v,na.rm=T)>1) stop(paste0("you have this twice in your input specification: ",v)) 
                  if(!is.na(v)) whichrow=which(rowSums(data.frame(ins)==v,na.rm=T)>0) else whichrow=which(rowSums(is.na(data.frame(ins)),na.rm=T)>0) #because if v is NA we have to treat it specially
                  o=outs[whichrow]
                  ifelse(length(o)==0,v,o)
                  #             browser()
                })} else {
                  for(v in unlist(ins)){
                    if(!is.na(v)) whichrow=which(rowSums(data.frame(ins)==v,na.rm=T)>0) else whichrow=which(rowSums(is.na(data.frame(ins)),na.rm=T)>0) #because if v is NA we have to treat it specially
                    o=outs[min(whichrow)]
                    #               warning(paste(ins))
                    #               warning(paste(outs))
                    if(!is.na(v)){
                      if(length(o)!=0 )  if(!is.na(v)) {this[this==v]=o} 
                    } else this[is.na(this)]=o
                  } 
                }
              
              
              if(ws[x,"setlevout"] %in% xc("ord nom")){
                #               browser()
                this=factor(this,ordered=ifelse(ws[x,"setlevout"]=="ord",T,F),levels=(mySheet[,1]))
              }
            }
            #           
            if(myblock[1,1]==x){
              writeWorksheetToFile(nn,concord,tabname,styleAction=XLC$"STYLE_ACTION.NONE")##fixme so only does itonce for multiples
            }
            #           assign(tabname,myvars,envir=.GlobalEnv)#if it is the last one in the block, ensure that mydata[,tabname] gets this block
          }
          if(y=="setlevout"){
            if(xmb(ws[x,y]))next()
            if(ws[x,y]=="str")this=as.character(this)
            if(ws[x,y]=="nom")this=(factor(this,ordered=F)) #not sure what happens if there was already a recode above
            if(ws[x,y]=="ord" & xmb(ws[x,"recode"])){this=(factor(this,ordered=T))}# if you provided a recode, this has already been done.
            if(ws[x,y]=="int")this=as.integer(this)
            if(ws[x,y]=="con")this=as.numeric(this)
          }
        }
        
        if(length(this)!=nrow(tmp)) {warning(paste0("Looks like you have a data frame within your data frame?",x));}
        #    
        
        l[[x]]=this;l<<-l
        names(l)[ncol(l)]=ws[x,"varnames"]
      } 
      #     l=l[,-1];
      l<<-l
      l=data.frame(l)
      #     colnames(l)=rownames(ws)
      rownames(l)=rownames(tmp)
      orphans=data.frame(tmp[,!(colnames(tmp)  %in% colnames(l))])
      colnames(orphans)=colnames(tmp)[which(!(colnames(tmp)  %in% colnames(l)))]
      l=data.frame(l,orphans) # to add in any new rows in data tmp which were not in ws
      #     assign(nam,l,envir=.GlobalEnv)#dont think this is necessary
    }  
    #   warning(sapply(l,attributes))
    warning("rewriting props file from R object")
    # xx<<-l$Spol
    # yy<<-data$Spol
    # stop()
    if(winner!="new"){  
      for(j in 1:ncol(l)){
        i=colnames(l)[j]
        store=attributes(l[,i])
        attributes(l[,i])=ws[i,]
        attr(l[,i],"ncol")=j
        attr(l[,i],"varnames")=i #to make sure the varnames are always correct
        ws[i,"varnames"]=i
        ws[i,"ncol"]=j
        # if(j==10)#       warning(ws[i,"ncol"])
        if(attr(l[,i],"setlevout")%in% xc("ord nom")) attr(l[,i],"levels")=store$levels
        if(attr(l[,i],"setlevout")%in% xc("ord nom")) attr(l[,i],"class")=store$class
      } 
      
      newWS=data.frame(ws)
      kill<<-newWS
    } else {
      
      oo=t(data.frame(oprop1(l)))
      newWS=ws=data.frame(oo)
    }  
    
    
    lold=l
    specials=setdiff(colnames(ws),labnames)
    nspecials=specials[str_sub(specials,1,1)!="."]
    attr(l,"specials")=specials
    #   warning(winner)
    
    
    if(set_doclusters){  for(special in attr(l,"specials")){
      #   
      l=makeClus(l,att=special,match="x")
      
    }}
    #   browser()
    attr(l,"specials")=specials
    specials<<-specials #fixme hack needed for xpairs steve
    
    # stop()
    if(winner!="new"){ 
      for(j in 1:ncol(l)){
        i=make.names(colnames(l))[j]
        if(!(i %in% colnames(lold) )){
          #         
          attr(l[,i],"ncol")=j
          attr(l[,i],"varnames")=i #to make sure the varnames are always correct
          
          ws[i,"varnames"]=ws[i,"label"]=ws[i,"shortlabs"]=i
          ws[i,"ncol"]=j
          ws[i,"setlevout"]=attr(l[,i],"setlevout")
          
          
        } #it must have come from cluster addition and wasn't already covered
      } 
      colnames(l)=ifelse(is.na(ws[,"newvarnames"]),colnames(l),ws[,"newvarnames"])
      
      newWS=data.frame(ws)
      kill<<-newWS
    } 
    #   stop(paste(colnames(newWS)))
    writeWorksheetToFile(nn,newWS,"props",styleAction=XLC$"STYLE_ACTION.NONE")
    system(paste0("wmctrl -c ",nn," - LibreOffice Calc"), wait=FALSE)
    
    browseURL(paste0(nn),browser="/usr/bin/libreoffice")  
    
    for(j in 1:ncol(l)){
      #     
      i=colnames(l)[j]
      al=attributes(l[,i])
      waln=((names(al) %in% nspecials)) 
      walnn=al[waln]
      walnnn=walnn!="NA"
      attr(l[,i],"anyspecials")=any(walnnn)
      #       browser()
      warning(i)
#       if(xmb(classer(l[,i])))     stop("something wrong with classer function")
      if(F&classer(l[,i])=="geo") {
        if(!all(xc("lat lon") %in% colnames(l))) stop(paste0("You asked for a geographical display for variable ",i," but you didn't provide variables lat and/or lon"))
        
        attr(l[,i],"lat")=unlist(l[,"lat"])
        attr(l[,i],"lon")=unlist(l[,"lon"])
      }
    }
    
#     browser()
    assign(paste0(nam,".r"),l,envir=.GlobalEnv)# update tmp adding new cols if necessary
    if(assigndata.r)assign(paste0("data.r"),l,envir=.GlobalEnv)# update tmp adding new cols if necessary
    #   stop("assinged data")
    saveRDS(l,paste0(nam,".r"))# update tmp adding new cols if necessary
    
    s=lapply(specials,function(x) x=findr(att=x,equ="list",datafilename=nam))
    names(s)=specials
    s<<-s
    assign(paste0(nam,".s"),s,envir=.GlobalEnv) #more general version when dealing with several datasets
    saveRDS(s,paste0(nam,".s"))# 


    blocks<<-unlist(na.omit(unlist(unique(lapply(data.r,function(i)attr(i,"block"))))))
    b<<-findr(equ="list",att="block",datafilename=nam)
    saveRDS(b,paste0(nam,".b"))# 
    assign(paste0(nam,".b"),b,envir=.GlobalEnv) #more general version when dealing with several datasets

    assign(paste0("specials"),specials,envir=.GlobalEnv)

#     saveRDS(specials,"specials")
  }    
  
}



ofix=function(x,  fixednames=xc("varnames ncol nt n1"),...){
    subx <- substitute(x)
    propname=paste0("prop.",subx)

  raw=oprop2(x)
  if(file.exists(propname)){
  more=readRDS(propname)
  rownames(more)=make.names(more[,1])
#   both=merge(raw[,fixednames],more[,setdiff(colnames(more),fixednames)],by=0,all.x=T)
  both=data.frame(raw[,fixednames],more[rownames(raw),setdiff(colnames(more),fixednames)],stringsAsFactors = F)
} else {
  both=raw
}
  if (is.name(subx)) 
      subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1L) 
      stop("'fix' requires a name")
    parent <- parent.frame()
    if (exists(subx, envir = parent, inherits = TRUE)) 
      both <- edit(both, title = subx, edit.row.names = FALSE,factor.mode="character",...)
#     else {
#       x <- edit(function() {
#       }, title = subx, ...)
#       environment(x) <- .GlobalEnv
#     }
    changed=x
    for(y in colnames(changed)){
#       browser()
      mostattributes(changed[,y])=as.list(both[y,])
    }
    changed=data.frame(changed,stringsAsFactors = F)
    changed=data.frame(lapply(changed,function(i){
      if(attr(i,"setlevout") %in% xc("n nom nominal")) i = factor(i,ordered=F)
      if(attr(i,"setlevout") %in% xc("o ord ordinal")) i = factor(i,ordered=T)
      if(attr(i,"setlevout") %in% xc("s str string")) i = as.character(i)
      if(attr(i,"setlevout") %in% xc("c con continuous")) i = as.numeric(i)
      i
    }),stringsAsFactors = F)

#     colnames(changed)=ifelse(sapply(both[,"newvarnames"],xmb),both[,"newvarnames"],colnames(changed))
    saveRDS(both,file = propname)
#     assign(subx, x, envir = .GlobalEnv)
    assign(propname, both, envir = .GlobalEnv)
    assign(paste0(subx,".raw"), x, envir = .GlobalEnv)
    assign(paste0(subx), changed, envir = .GlobalEnv)
    saveRDS(changed,paste0(subx,".raw"))
}


#' ofix also creates a properties object as well writing a corresponding properties file
#' but although you can edit the properties object you have to write it to the file to ensure reproducibility.
#' 
#' from the attribute
#' @param dat  dataset
#' @param att the attribute
#' @param equ the value of the attribute
#' @family experimental
#' @return A dataset
#' 
#' @examples Here are some examples
writeprop=function(object=prop.data,file="prop.data"){
  saveRDS(object,file)
}


oprop2=function(df,labnames=xc("varnames ncol nt n1 newvarnames label formula recode setlevout")){ #gets stuff from the data only
  #   df=get(nam,envir=.GlobalEnv)
  #   df<<-df
  colnames(df)=make.names(colnames(df),unique=T)
  ttt=sapply(colnames(df),function(xx){
    x=df[,xx]
    t=table(x)
    tt=names(t[rev(order(t))])
    sapply(labnames,function(y){
      
      if(y=="varnames") xx else {
        if(y=="ncol") min(which(xx==colnames(df))) else {
          if(y=="nt") length(unique(x)) else {
            if(y=="n1") paste(tt[1:min(10,length(tt))],collapse=";;") else {
              if(y=="n2") "" else {
                if(y=="n3") "" else {
                  if(y=="setlev") classer(x) else {
                    if(y=="setlevout") classer(x) else {
                      if(y=="label") ifelse(is.null(attr(x,"label")),xx,attr(x,"label")) else {
                        a=attr(x,y)
                        if(is.null(a)) '' else a
                      }}}}}}}}}
    })
  })
  t(ttt)
}#it gets the possibly properties
