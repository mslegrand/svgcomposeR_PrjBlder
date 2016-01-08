

#special tag handling

# in.defs.only.elements<-c("clipPath", "cursor", "filter", "linearGradient", "marker", "mask", "pattern", "radialGradient", "symbol")



feConvolveMatrixTagQuote<-quote(
  if(inherits(attrs$kernelMatrix,"matrix")){
    attrs$order<-paste(dim(attrs$kernelMatrix))
  }
)

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
  "feMergeNode", #added manually, need to investigate why this wasn't included!
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

# fe
feQuote<-quote({
  # Move the XfeNode elements to the rtv
  which(names(attrs)=='XfeNode')->indx
  if(length(indx)>0){
    tmp<-attrs[indx]
    attrs<-attrs[-indx]
    rtv<-c(rtv,tmp)
  }
  # pickout all indices if any an "in" attribute
  indx.in<-which(names(attrs)=='in' | names(attrs)=='in2')
  # for each in attribute, 
  for(n in indx.in){
    an<-attrs[[n]] #an is 'in' attribute n
    if (inherits(an, 'list') && length(an)>=1){ # if non-trivial list
      len<-length(an)
      rtv<-c(rtv, an[1:(len-1)]) #move into rtv the prefix
      feNode=an[[len]]} # an set feNode to an
    else{
      feNode=an
    }
    if(inherits(feNode, "XMLAbstractNode")){ #may want to require tag is fe!=feMergeNode??
      resultStr<-getsafeNodeAttr("result", feNode) #only if !=fe
      rtv<-c( rtv, XfeNode=feNode) #rtv biw contains all feNodes at that location
      attrs[[n]]<-resultStr  #attrs contains a link to that node          
    }
  }          
})

filterTagQuote<-quote(
  if("XfeNode" %in% names(args)){
    tmp<-names(args)
    indx<-which(tmp=="XfeNode")
    tmp[indx]<-""
    names(args)<-tmp
  }
)

defsTagQuote<-quote({
  tmp<-names(args) 
  indx<-which(tmp=="XdefsNode")
  tmp[indx]<-""
  names(args)<-tmp}
)

svgTagQuote<-quote({
  # add unnamed defs if defs not an unnamed arg
})



#template for different quotes
makeSpecTr<-function(aName, aElements, aMssg){
  ptree<-substitute(  
  if( aName %in%  names(attrs) ){
    # grab all occurances, and proccess each
    indx<-which(names(attrs) ==aName)
    for( n in indx){
      aNode<-attrs[[n]]
      if(inherits(aNode, "XMLAbstractNode")){
        if(!(xmlName(aNode) %in% aElements)){ 
          stop(aMssg)
        }
        fid<-getsafeNodeAttr("id",aNode)
        #rtv<-c(rtv,XdefsNode=aNode)
        rtv<-c(rtv, aNode)
        attrs[[n]]=paste0("url(#",fid,")")
      }  
    }  
  }, list(aName=aName, aElements=aElements, aMssg=aMssg))
  ptree
}

# looks like filterQuote can be generated using makeSpecTr
filterQuote<-quote(if( "filter" %in%  names(attrs) ){
  # grab all occurances, and proccess each
  indx<-which(names(attrs) =="filter") 
  #cat("indx=",indx,"\n")
  for( n in indx){
    filterNode<-attrs[[n]]
    if(inherits(filterNode, "XMLAbstractNode")){
      if(xmlName(filterNode)!="filter"){ # check tag name
        stop("Not a filter node")
      }
      fid<-getsafeNodeAttr("id",filterNode)
      rtv<-c(rtv,filterNode)
      attrs[[n]]=paste0("url(#",fid,")")
    }  
  }  
}
)

textQuote<-quote(if(!is.null(names(attrs))){
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
})


gradientColorQuote<-quote(
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
  })

 

fillQuote<-makeSpecTr(aName="fill", aElements = c("pattern", "linearGradient", "radialGradient"), aMssg="Bad fill parameter")
markerEndQuote<-makeSpecTr(aName="marker-end", aElements = "marker", aMssg="Bad marker parameter")
markerMidQuote<-makeSpecTr(aName="marker-mid", aElements = "marker", aMssg="Bad marker parameter")
markerStartQuote<-makeSpecTr(aName="marker-start", aElements = "marker", aMssg="Bad marker parameter")
maskQuote<-makeSpecTr(aName="mask", aElements = "mask", aMssg="Bad mask")
clipPathQuote<-makeSpecTr(aName="clip-path", aElements = "clipPath", aMssg="Bad clipPath parameter")


#todo: migrate to specialTreatments
specialTreatments<-list(
    fill=makeSpecTr(aName="fill", aElements = c("pattern", "linearGradient", "radialGradient"), aMssg="Bad fill parameter"),
    markerEnd=makeSpecTr(aName="marker-end", aElements = "marker", aMssg="Bad marker parameter"),
    markerMid=makeSpecTr(aName="marker-mid", aElements = "marker", aMssg="Bad marker parameter"),
    markerStart=makeSpecTr(aName="marker-start", aElements = "marker", aMssg="Bad marker parameter"),
    mask=makeSpecTr(aName="mask", aElements = "mask", aMssg="Bad mask"),
    clipPath=makeSpecTr(aName="clip-path", aElements = "clipPath", aMssg="Bad clipPath parameter"),
    filter=makeSpecTr(aName="filter", aElements = "filter", aMssg="Not a filter node"),
    text=quote(if(!is.null(names(attrs))){
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
    gradientColor=quote(
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
      })
)

# makeAni<-function(etag, aaCombos){
#   #etag is the tag element tag (animate, set)
#   #aaCombos is all the animate combos
#   c(
#     substitute(combos<-aaCombos, list(aaCombos=aaCombos )),
#     quote(attributeName<-args[["attributeName"]]),
#     substitute(
#       if( !is.null(attributeName) && attributeName %in% names(combos) ){
#         attributeNames<-combos[[attributeName]]
#         N<-length(attributeNames)
#         tmp<-lapply( 1:N, function(i){
#           args2<-args
#           args2[["attributeName"]]<-attributeNames[i]
#           tmp<-c("from","to","values")
#           ind<-intersect(names(args),tmp)
#           args2[ind]<-lapply(args2[ind], function(vec){
#             j<-min(i,length(vec))
#             vec[j]
#           })              
#           etag(args2)
#         })
#         return(tmp)    
#       },
#       list(etag=etag, aaCombos=aaCombos)
#     )  
#   )
# }


makeAni<-function(etag, aaCombos){
  eFns<-list(set=quote(set), animate=quote(animate) )
  fn<-eFns[[etag]]
  c(
    substitute(combos<-aaCombos, list(aaCombos=aaCombos )),
    quote(attributeName<-args[["attributeName"]]),
    "# combo should have (values) Xor (from or to)",
    quote(if(is.null(attributeName)) stop("missing attributName in animation")),
    quote( aNames <- combos[[attributeName]]),  #for example aNames=c("x","y","z"),
    substitute(
      if(!is.null(aNames) ){  
        # 1 eList={from, to , values}
        eListNames<-intersect(names(args), c("from", "to", "values"))
        eList<-sapply(eListNames, function(an){
          vals<-args[[an]]
          if(an=="values"){
            if(inherits(vals, "character")){
              vals<-paste(vals,collapse=";")
              vals<-strsplit(vals, ";")[[1]]
              vals<-vals[ grepl("[0-9]", vals) ]
              vals<-as.list(vals)
              vals<-strsplit(vals, "[ ,]+")[[1]]
            }
            vals<-extractValues(vals, aNames)
          } else { #an== from or to
            if(inherits(vals, "character")){
              vals<-paste(vals, collapse=" ")
              vals<-strsplit(vals, "[ ,]+")[[1]]
            }
            vals<-as.list(vals)
            if(!(length(vals)[1]==length(aNames) )) stop(paste0("animated combo attribute has incorrect '",an,"' count"))
            names(vals)<-aNames
          }
          vals }, simplify = FALSE, USE.NAMES = TRUE)
        rtv<-lapply( aNames,   function(an){
          args2<-args
          args2[["attributeName"]] <- an
          eListAN<-lapply(eList, function(el){
            el[[an]]
          } )
          args2[eListNames]<-eListAN
          node<-fn(args2)
          node
        })
        return(rtv)
      },
      list(fn=fn, aaCombos=aaCombos)    
    )
  )
}


codeSeg<-list(

)



