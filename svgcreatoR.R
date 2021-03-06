
#utils:::.addFunctionInfo(svg=c("cat","dog"))

library(data.table)
library(XML)
if(!exists("requireTable")){ source("tableLoader.R") }
source("specialTagHandlers.R")

#notess to self
#todo:
# add param processing to doc[['id']]: will need to consider tag that id belongs to or allow anything
# add param processing for use: same problem as #1, restricted to params for svg, symbol, g, graphics elements
# add param processing for animate: same problem as #1 restricted to animateable params
# all boil down to given reg attribute, preprocess, given pres attribute preprocess


# Some questions 
#1. how to add function completion:
# i) utils:::.addFunctionInfo(fn=c("cat","dog")) #note 3 colons
# ii)alternatively: 
#    pkgEnv = getNamespace("MyPackage")
#    attach(pkgEnv)
#2. How to add function documentation???


# Builds the svgFnQ stuff
build.svgFnQ<-function(){
 
  requireTable(AET.DT, COP1.DT, PA.DT)
#   
  # all elements
  ele.tags<-unique(AET.DT$element)
  #all attributes
  ele.tags.attributeName<-AET.DT[attr=="attributeName"]$element
  
  # here we get the special cases for our quotes
  attrsEle2Quote<-list(
    filter=c("g",PA.DT[attr=='filter' & variable=='Applies to']$value),
    fill=c("g",PA.DT[attr=='fill' & variable=='Applies to']$value),
    clip.path=c("g",PA.DT[attr=='clip-path' & variable=='Applies to']$value),
    mask=c("g",PA.DT[attr=='mask' & variable=='Applies to']$value),
    marker=c("g",PA.DT[attr=="marker properties" & variable=='Applies to']$value)
  )
  
  # build list of all combos for potential animation
  COP1.DT[,.(variable,value)]->COP2.DT
  split(COP2.DT$value, COP2.DT$variable)->tmp
  lapply(tmp,unique)->aaCombos
  aaCombos[["in1"]]<-NULL
  
  #helper function
  centerable<-function(ele.tag, AET.DT){
    ifelse(
      nrow(AET.DT[  element==ele.tag & 
                      (attr=='x' | attr=='y' | attr=='width' | attr=='height') ,]
      )==4,
      "attrs<-mapCenteredXY(attrs)",
      ""
    )  
  }
  
  #helper function
  qcomboParamsFn<-function(etag){
    tmp<-COP1.DT[element==etag]
    if(nrow(tmp)>0){
      cp.list<-split(tmp$value, tmp$variable)
      # for each element of tmp.list, add the appropriate quote
      substitute(attrs<-comboParamHandler(attrs, cp ), list(cp=cp.list))
    } else {
      quote(NULL)
    }
  }
       
  createEleFnQ<-function(ele.tag, AET.DT){
    AET.DT[element==ele.tag & treatValueAs!="ignore",]->ele.dt
    ele.dt[, paste(attr, collapse=" "), by=treatValueAs]->treat_attrs.dt
    
    #helper fn
    animateComboParam<-function(ele.tag){
      if(ele.tag %in% c("set","animate")){
        body0<-append(body0, makeAni(ele.tag, aaCombos) ,2)
      } else {
        NULL
      }
    }
    
    #helper fn
    insertConditionalCode<-function(ele.tag, ele.tag.set, fn, ...){
      if(ele.tag %in% ele.tag.set){
        fn(ele.tag, ...)
      } else {
        NULL
      }     
    }
    
    #helper fn
    echoQuote<-function(ele.tag, q){
      q
    }
    
  #Each fn body starts with
    body0<-c(
      quote( args <- list(...) ),
      quote( args <- promoteUnamedLists(args) ),
      insertConditionalCode(ele.tag,c('set', 'animate'),makeAni, aaCombos),
      insertConditionalCode(ele.tag,'filter', echoQuote, filterTagQuote),
      quote( attrs <- named(args) ),
      insertConditionalCode(ele.tag,'feConvolveMatrix', echoQuote, feConvolveMatrixTagQuote)   
    )

  # call comboParamHandler combo params for given ele.tag
    ppXtraCL<-list( qcomboParamsFn(ele.tag) )
       
    if(nrow(AET.DT[element==ele.tag & (attr=='x' | attr=='y' | attr=='width' | attr=='height') ,])==4 ){
      ppXtraCL<-c(ppXtraCL, quote(attrs<-mapCenteredXY(attrs) ) ) # append a call
    }
    
  # process elements which contain attributeName as an attribute
    if(ele.tag %in% ele.tags.attributeName){
      ppXtraCL<-c(ppXtraCL, quote(attrs<-mapAttributeName(attrs)))
    }
      
    ppXtraCL[sapply(ppXtraCL, is.null)] <- NULL #remove any nulls #?move to end????
    body1<-ppXtraCL
    
    #Insert special handling for animate element here
    if(ele.tag == "animate"){
      body1<-c(body1, quote(attrs<-preProcAnimate(attrs) ) )
    }
         
  # add code to treat special lists, ie. comma list, space list, semicolon list ...
  # This becomes body2
    split(treat_attrs.dt, rownames(treat_attrs.dt))->tmp # (convert rows of treat_attrs.dt table to list)  
    preprocAttrValueFn<-function(tvaAttr){
      c(
        substitute( indx<-sapply(names(attrs),function(x)grepl(paste('(^| )',x,'($| )',sep=''), V1 )),tvaAttr),      
        substitute( if(length(indx)>0){ attrs[indx]<-lapply(attrs[indx], function(x){ svgPreproc[[treatValueAs]](x) })}, tvaAttr)
      )
    } 
    body2<-lapply(tmp, function(tvaAttr){preprocAttrValueFn(tvaAttr)}) 
    unlist(body2, use.names=F)->body2
    as.list(body2)->body2

# **  This is necessary  for filter, feElements, etc. to return a list !!!
    body2<-c(body2, 
             quote(rtv<-list()), #rtv begins life here and is populated by the following
             insertConditionalCode(ele.tag,attrsEle2Quote$filter, echoQuote, filterQuote),
             insertConditionalCode(ele.tag,attrsEle2Quote$fill, echoQuote, fillQuote),
             insertConditionalCode(ele.tag,attrsEle2Quote$clip.path, echoQuote, clipPathQuote),
             insertConditionalCode(ele.tag,attrsEle2Quote$mask, echoQuote, maskQuote),
             insertConditionalCode(ele.tag,attrsEle2Quote$marker, echoQuote, markerEndQuote),
             insertConditionalCode(ele.tag,attrsEle2Quote$marker, echoQuote, markerMidQuote),
             insertConditionalCode(ele.tag,attrsEle2Quote$marker, echoQuote, markerStartQuote),
             insertConditionalCode(ele.tag, c('text' , 'textPath' , 'tspan'), echoQuote, textQuote),
             insertConditionalCode(ele.tag, c("linearGradient",  "radialGradient"), echoQuote, gradientColorQuote)            
    )

    #add code to add to node children and node from tag
    body3<-substitute(node<-newXMLNode(ele.tag, attrs=attrs, .children=allGoodChildern(args),
                      suppressNamespaceWarning=getOption("suppressXMLNamespaceWarning", TRUE)), 
                      list(ele.tag=ele.tag)
    )

    body3<-c(body3,
             # **  add this for filter, feElements, etc.
             quote({ 
               if(length(rtv)>0){
                 node<-c(rtv,node)
               }
               node
             })
    )

# ** prior to adding .children and attrs, we process for our custom
    # attribute=element assignments
    
    if(ele.tag %in% filterElementTags){
      body3<-c(
        feQuote, # moves  feElements form args to rtv prior to node creation,
        body3, 
        quote(node<-c(rtv,node)) # returns an rtv list + node
      )   
    }

    fn<-function(...){}
    body(fn)<-as.call(c(as.name("{"), body0, body1, body2, body3))
    fn  
    
  }
  
  svgFnQ<-lapply(ele.tags, createEleFnQ, AET.DT=AET.DT )
  names(svgFnQ)<-ele.tags

  #here we handle element names with -
  indx<-grep("-", names(svgFnQ))
  tmpFn<-svgFnQ[indx]
  names(tmpFn)<-gsub("-",".",names(tmpFn))
  svgFnQ<-c(svgFnQ, tmpFn,
    list(
      getNode=function(rootNode,id){
        if(id!='root'){
          kidV <- getNodeSet(rootNode, paste("//*[@id=\"", id, "\"]", sep=""))
        } else {
          kidV <- list(rootNode)
        }
        if (length(kidV)==0){
          stop("Cannot find node with id=",id)
        }
        kidV
      },
      script=function(...){
        args <- list(...)
        stopifnot( length(args)>0 , sapply(args, function(x)inherits(x,"character")))
        paste(args,collapse="\n")->js
        newXMLNode('script', attrs= list(type="text/JavaScript"),
                   newXMLCDataNode(js), 
                   suppressNamespaceWarning = getOption("suppressXMLNamespaceWarning",                                                   TRUE)
        )
      },
      translate=function(x,y=NULL){
        if(length(c(x,y)!=2)){
          stop("bad translate arguments")
        }
        list(translate=c(x,y))
      },
      rotate=function(angle, x=NULL, y=NULL){
        if(!(length(c(angle,x,y)) %in% c(1,3))){
          stop("bad rotate arguments")
        }
        list(rotate=c(angle,x,y))     
      },
      rotatR=function(angle, x=NULL, y=NULL){
        if(!(length(c(angle,x,y)) %in% c(1,3))){
          stop("bad rotate arguments")
        }
        tmp<-c(angle,x,y)
        tmp[1]<-as.numeric(tmp[1])*180/pi #convert from radians to degrees
        list(rotate=tmp)     
      },
      scale=function(x,y=NULL){
        if(length(c(x,y)!=2)){
          stop("bad translate arguments")
        }
        list(scale=c(x,y))
      }
    )
  )
  svgFnQ
}
# svgFnQ<-build.svgFnQ()

#svgFnQ$script<-function(...){
  #args <- list(...)
  #stopifnot( length(args)>0 , sapply(args, function(x)inherits(x,"character")))
  #paste(args,collapse="\n")->js
  #newXMLNode('script', attrs= list(type="text/JavaScript"),
             #newXMLCDataNode(js), 
             #suppressNamespaceWarning = getOption("suppressXMLNamespaceWarning",                                                   TRUE)
  #)
#}

#type="text/JavaScript"


