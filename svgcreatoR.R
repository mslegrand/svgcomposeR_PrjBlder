
#utils:::.addFunctionInfo(svg=c("cat","dog"))

library(data.table)
library(XML)

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
    #ele.treatments<-unique(ele.dt$treatValueAs)
    ele.dt[, paste(attr, collapse=" "), by=treatValueAs]->treat_attrs.dt
    #This is the extras 
    body0<-c(
      quote( args <- list(...) ),
      quote( args <- promoteUnamedLists(args) ),
      quote( attrs <- named(args) )
    )
    
# in.defs.only.elements<-c("clipPath", "cursor", "filter", "linearGradient", "marker", "mask", "pattern", "radialGradient", "symbol")
    #preproces 'filter=filter' here
    # special case for filter assignment
#     if( "filter" %in%  names(args) ){
#       # grab all occurances, and proc each
#       indx<-which(names(args) =="filter") 
#       for( n in indx){
#         filterNode<-arg[[n]]
#         if(inherits(filterNode, "XMLAbstractNode")){
#           if(xmlName(filterNode)!="filter"){ # check tag name
#             stop("Not a filter node")
#           }
#           defsNode<-getDefsNode(doc) #or parent, will need to work this usage out!!!!
#           fid<-getsafeNodeAttr("id",filterNode)
#           addFilter2Defs(defsNode, filterNode)
#           args[[i]]=paste0("url(#",fid,")")
#         }  
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

    #add code to add to node children
    body3<-substitute(node<-newXMLNode(ele.tag, attrs=attrs, .children=allGoodChildern(args),
                      suppressNamespaceWarning=getOption("suppressXMLNamespaceWarning", TRUE)), 
                      list(ele.tag=ele.tag))
    
    #special cases for text (may replace this later)
    if(ele.tag %in% c('text' , 'textPath' , 'tspan')){
      body3<-c(
        quote(if(!is.null(names(attrs))){
          attr.names<-names(attrs)
          attr.names<-gsub("^(((style))|((weight))|((variant))|((size))|((family)))$", "font-\\1",attr.names, fixed=F)
          attr.names<-gsub("^anchor$","text-anchor",attr.names)
          names(attrs)<-attr.names
          if(!is.null(attrs[["cxy"]])){
            attrs[["text-anchor"]]<-'middle'
            attrs[["dominant-baseline"]]="central"
            attrs[["xy"]]=attrs[["cxy"]]
            attrs[["cxy"]]=NULL
          }
          attrs<-mapArg(attrs,"xy", c("x","y"))
          text<-NULL
          if("text" %in% attr.names){ ### use value instead of text???
            text<-attrs["text"]
            attrs["text"]<-NULL
          }
        }),
        body3
      )    
    }
    #special code for gradients
    if(ele.tag %in% c("linearGradient",  "radialGradient")){
      body3<-c(
        quote(
          if("colors" %in% names(attrs)){
            colors<-attrs[["colors"]]
            attrs[["colors"]]<-NULL
            if("offsets" %in% names(attrs)){
              offsets<-attrs[["offsets"]]
              attrs[["offsets"]]<-NULL
            } else {
              offsets<-seq(0,100,length.out=length(colors))
            }
            for(i in 1:length(colors)){
              attrs.si<-list(offset=sprintf("%d%%", as.integer(offsets[i])), "stop-color"= colors[i])
              stopi<-newXMLNode("stop", attrs=attrs.si)
              args<-c(args,stopi)
            }
          }),
        body3
      )    
    } 
    #handle filter=fnode, this requires doc, parentNode, fnode
    
    
#     if(ele.tag %in% c("animate")){
#       #special handling of by, from to depending on attributeName/type
#       
#     }
#     if(ele.tag %in% c("use")){
#       #allow for anything
#     }  

# in.defs.only.elements<-c("clipPath", "cursor", "filter", "linearGradient", "marker", "mask", "pattern", "radialGradient", "symbol")


  # special cases for fe (filter elements)
  filterElementTags<-c(
    "feBlend", 
    "feColorMatrix",
    "feComponentTransfer",
    "feComposite",
    "feConvolveMatrix",
    "feDiffuseLighting",
    "feDisplacementMap",
    "feFlood",
    "feGaussianBlur",
    "feImage",
    "feMerge",
    "feMorphology",
    "feOffset",
    "feSpecularLighting",
    "feTile",
    "feTurbulence"
  )

feElementsIn<-c(
  'feConvolveMatrix','feDiffuseLighting','feOffset',
  'feBlend','feColorMatrix','feComponentTransfer',
  'feComposite','feDisplacementMap','feGaussianBlur',
  'feMorphology','feSpecularLighting','feTile')

  if(ele.tag %in% filterElementTags){
      body3<-c(
        quote({
          indx.in<-which(names(attrs)=='in' | names(attrs)=='in2')
          rtv<-list()
          for(n in indx.in){
            an<-attrs[[n]]
            if (inherits(an, 'list') && length(an)>=1){ 
              len<-length(an)
              rtv<-c(rtv, an[1:(len-1)])
              feNode=an[[len]]
              if(inherits(feNode, "XMLAbstractNode")){ #may want to require tag is fe??
                resultStr<-getsafeNodeAttr("result", feNode)
                rtv<-c( rtv, feNode )
                attrs[[n]]<-resultStr            }
            }
          }          
        }
        ),
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




