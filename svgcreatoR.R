
#utils:::.addFunctionInfo(svg=c("cat","dog"))

library(data.table)
library(XML)
source("specialTagHandlers.R")
#todo:
# add param processing to doc[['id']]: will need to consider tag that id belongs to or allow anything
# add param processing for use: same problem as #1, restricted to params for svg, symbol, g, graphics elements
# add param processing for animate: same problem as #1 restricted to animateable params
# all boil down to given reg attribute, preprocess, given pres attribute preprocess

#todo:
# 5. Add function completions HOW???
# 6. Add documentation HOW???
# 7. Add sample programs 
# 8. Add unit testing (regression testing?) and other methods
# 9.  Animation:
#    doc['initialize']
#    doc["set"]( color(id)="red", ...)
#    doc["animate"]( XY(id)=c(1,2), xy(id)=(1,2), ...)
#    or 
#    doc[[id]], replace by doc$build(rootNode=id,...) 
#           or build(doc, rootNode=id,  ...)
#           or doc$root(...)
#    doc["set"], replace by doc$set(...) or  set(doc, ...)
#    doc["animate], replace by doc$animate(...) or animate(doc, ...)

#   Do we need 
#       1. ids(doc) ? (return all ids)
#       2. def(doc) ? (return ids of defs)
#       3. svgnode(id) ?
#       4. 
#       

# for filter should we always append % to the x,y width, heigth values???
# k for k1,k2,k3,k4 in feComposite ?

#1. how to add function completion:
# i) utils:::.addFunctionInfo(fn=c("cat","dog")) #note 3 colons
# ii)alternatively: 
#    pkgEnv = getNamespace("MyPackage")
#    attach(pkgEnv)
#2. How to add function documentation???


# The following line is required because of a bug in devtools 
# (maybe they will fix it some day)
#.datatable.aware=TRUE

# if(!exists("AET.DT")){
#   fread("./dataTables/AETTable.tsv")->AET.DT  
# }
# if(!exists("COP.DT")){
#   fread("dataTables/comboParams.tsv")->COP.DT
# }

# preproc.treat.val.as<-function(v){
#   tmp<-c(
#     "cmm-list\\s+\\{4\\}"="cmm-list",
#     "default"="ignore",
#     "filterprimitiveinattribute"="ignore",
#     "integer"="ignore",
#     "pointsbnf"="cmm-wsp-list",
#     "transformlist"="transform-list",
#     "lengths"="wsp-list",
#     "numbers"="wsp-list",
#     "coordinates"="wsp-list",
#     "special-string"="wsp-list",
#     "funciri"="ignore")
#   
#   for( n in names(tmp)){
#     v<-gsub(n, tmp[n], v)
#   }
#   v  
# }

#preprocess AVEL.DT
# AVEL.DT[,treatValueAs:=preproc.treat.val.as(treatValueAs)]
# AVEL.DT[,list(attr,element,anim,treatValueAs)]->AET.DT



# Builds the svgFnQ stuff
build.svgFnQ<-function(){
  if(!exists("AET.DT")){
    fread("./dataTables/AETTable.tsv")->AET.DT  
  }
  if(!exists("COP.DT")){
    fread("dataTables/comboParams.tsv")->COP.DT
  }
  if(!exists("PA.DT")){
    fread("dataTables/presentationAttr.tsv")->PA.DT
  }
  
  ele.tags<-unique(AET.DT$element)
  
  ele.tags.attributeName<-AET.DT[attr=="attributeName"]$element
  
  centerable<-function(ele.tag, AET.DT){
    ifelse(
      nrow(AET.DT[  element==ele.tag & 
                      (attr=='x' | attr=='y' | attr=='width' | attr=='height') ,]
      )==4,
      "attrs<-mapCenteredXY(attrs)",
      ""
    )  
  }
    
  # "ignore cmm-list path-data-list wsp-list scln-list cmm-scln-list number-optional-number cln-scln-list cmm-wsp-list transform-list"
  createEleFnQ<-function(ele.tag, AET.DT){
    AET.DT[element==ele.tag & treatValueAs!="ignore",]->ele.dt
    ele.dt[, paste(attr, collapse=" "), by=treatValueAs]->treat_attrs.dt
    #This is the extras 
    body0<-c(
      quote( args <- list(...) ),
      quote( args <- promoteUnamedLists(args) ),
      quote( attrs <- named(args) )
    )
    if(ele.tag=="filter"){
      body0<-append(body0,filterTagQuote,2)
    } 
#     else {
#       if( ele.tag=='defs'){
#         body0<-append(body0,defsTagQuote,2)
#       }
#     }
    
    qcomboParamsFn<-function(etag){
      tmp<-COP.DT[element==etag]
      if(nrow(tmp)>0){
        cp.list<-split(tmp$value, tmp$variable)
        # for each element of tmp.list, add the appropriate quote
        substitute(attrs<-comboParamHandler(attrs, cp ), list(cp=cp.list))
      } else {
        quote(NULL)
      }
    }
        
    ppXtraCL<-list( qcomboParamsFn(ele.tag) )
       
    if(nrow(AET.DT[element==ele.tag & (attr=='x' | attr=='y' | attr=='width' | attr=='height') ,])==4 ){
      ppXtraCL<-c(ppXtraCL, quote(attrs<-mapCenteredXY(attrs) ) ) # append a call
    }

    if(ele.tag %in% ele.tags.attributeName){
      ppXtraCL<-c(ppXtraCL, quote(attrs<-mapAttributeName(attrs)))
    }
      
    ppXtraCL[sapply(ppXtraCL, is.null)] <- NULL #remove any nulls
    body1<-ppXtraCL
    
    #Insert special handling for animate here
    if(ele.tag == "animate"){
      cat("hello\n")
      body1<-c(body1, quote(attrs<-preProcAnimate(attrs) ) )
    }
         
    # add code to treat special lists, ie. comma list, space list, semicolon list ...
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

    
# **  add this for filter, feElements, etc.
    body2<-c(body2, quote(rtv<-list()))

    #add code to add to node children
    body3<-substitute(node<-newXMLNode(ele.tag, attrs=attrs, .children=allGoodChildern(args),
                      suppressNamespaceWarning=getOption("suppressXMLNamespaceWarning", TRUE)), 
                      list(ele.tag=ele.tag)
    )
# in.defs.only.elements<-c("clipPath", "cursor", "filter", "linearGradient", "marker", "mask", "pattern", "radialGradient", "symbol")
# in.defs.only.elements<-c("clipPath", "cursor", "filter", "linearGradient", "marker", "mask", "pattern", "radialGradient", "symbol")
# **  add this for filter, feElements, etc.
    body3<-c(body3,
             quote({
               if(length(rtv)>0){
                 node<-c(rtv,node)
               }
               node
             })
    )

# ** prior to adding .children and attrs, we process for our custom
    # attribute=element assignments
    
    # here we get the special cases for our quotes
    attrsEle2Quote<-list(
      filter=c("g",PA.DT[attr=='filter' & variable=='Applies to']$value),
      fill=c("g",PA.DT[attr=='fill' & variable=='Applies to']$value),
      clip.path=c("g",PA.DT[attr=='clip-path' & variable=='Applies to']$value),
      mask=c("g",PA.DT[attr=='mask' & variable=='Applies to']$value),
      marker=c("g",PA.DT[attr=="marker properties" & variable=='Applies to']$value)
    )
    
    #todo change this to consider only elements which can have a filter attribute 
    # filter is a presentation attribute, so wc3 seems to say that
    # filter can work on svg, defs and 40 or more other stuff
    # Using PA.DT we see 20 elements
    # PA.DT[attr=='filter' & variable=='Applies to']$value
    # not sure what filter in animate does
    #if(!(ele.tag %in% c('svg', 'defs'))){
    if(ele.tag %in% attrsEle2Quote$filter){
        body3<-c(filterQuote,body3)
    }
    # PA.DT[attr=='fill' & variable=='Applies to']$value (12 ele)
    # PA.DT[attr=='clip-path' & variable=='Applies to'] (22 ele)
    # PA.DT[attr=='mask' & variable=='Applies to'] (21 ele)
    #if(TRUE){ #shapes, and what else? all are presentation attrs
#       body3<-c(fillQuote, clipPathQuote, maskQuote, body3)
#     } 
    if(ele.tag %in% attrsEle2Quote$fill){
      body3<-c(fillQuote,body3)
    }
    if(ele.tag %in% attrsEle2Quote$clip.path){
      body3<-c(clipPathQuote,body3)
    }
    if(ele.tag %in% attrsEle2Quote$mask){
      body3<-c(maskQuote,body3)
    }
    
    # all presentation attrs
    # should be four elements
    #PA.DT[attr=="marker properties" & variable=='Applies to']
    #if(ele.tag %in% c('line', 'polyline', 'path', 'polygon')){ # and what else?
    if(ele.tag %in% attrsEle2Quote$marker){ # and what else?
        body3<-c(markerEndQuote, markerMidQuote, markerStartQuote, body3)
    } 
    #special cases for text (may replace this later)
    if(ele.tag %in% c('text' , 'textPath' , 'tspan')){
      body3<-c(textQuote, body3)    
    }
    #special code for gradients
    if(ele.tag %in% c("linearGradient",  "radialGradient")){
      body3<-c(gradientColorQuote, body3 )    
    } 
    
#     if(ele.tag %in% c("animate")){
#       #special handling of by, from to depending on attributeName/type
#       
#     }
#     if(ele.tag %in% c("use")){
#       #allow for anything
#     }  

  if(ele.tag %in% filterElementTags){
      body3<-c(
        feQuote,
        body3,
        quote(node<-c(rtv,node))
      )   
    }

    fn<-function(...){}
    body(fn)<-as.call(c(as.name("{"), body0, body1, body2, body3))
    fn  
    
  }
  
  svgFnQ<-lapply(ele.tags, createEleFnQ, AET.DT=AET.DT )
  names(svgFnQ)<-ele.tags

  
  
  
  #here we handle names with -
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
              }
  )
  )
  svgFnQ
}

svgFnQ<-build.svgFnQ()

svgFnQ$script<-function(...){
  args <- list(...)
  stopifnot( length(args)>0 , sapply(args, function(x)inherits(x,"character")))
  paste(args,collapse="\n")->js
  newXMLNode('script', attrs= list(type="text/JavaScript"),
             newXMLCDataNode(js), 
             suppressNamespaceWarning = getOption("suppressXMLNamespaceWarning",                                                   TRUE)
  )
}

#type="text/JavaScript"


