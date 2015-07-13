

#special tag handling

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

# fe
feQuote<-quote({
  indx.in<-which(names(attrs)=='in' | names(attrs)=='in2')
  #rtv<-list()
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
})

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
      #print(filterNode)
      fid<-getsafeNodeAttr("id",filterNode)
      #cat("fid=",fid,"\n")
      #may want to change later, but for now just promote
      rtv<-c(rtv,filterNode)
      #add filter to the return set
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

#TODO

# - clipPath: clip-path="url(#MyClip)"
# - mask:  mask = mask



fillQuote<-quote(if( "fill" %in%  names(attrs) ){
  # grab all occurances, and proccess each
  indx<-which(names(attrs) =="fill") 
  for( n in indx){
    aNode<-attrs[[n]]
    if(inherits(aNode, "XMLAbstractNode")){
      if(!(xmlName(aNode) %in% c("pattern", "linearGradient", "radialGradient")))
        { # check tag name
        stop("Bad fill parameter")
      }
      fid<-getsafeNodeAttr("id",aNode)
      rtv<-c(rtv,aNode)
      attrs[[n]]=paste0("url(#",fid,")")
    }  
  }  
}
)

#marker-end, marker-middle, marker-start
# markerEndQuote<-quote(if( "marker-end" %in%  names(attrs) ){
#   # grab all occurances, and proccess each
#   indx<-which(names(attrs) =="marker-end") 
#   for( n in indx){
#     aNode<-attrs[[n]]
#     if(inherits(aNode, "XMLAbstractNode")){
#       if(!(xmlName(aNode) %in% c("marker")))
#       { # check tag name
#         stop("Bad marker parameter")
#       }
#       fid<-getsafeNodeAttr("id",aNode)
#       rtv<-c(rtv,aNode)
#       attrs[[n]]=paste0("url(#",fid,")")
#     }  
#   }  
# }
# )

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
        rtv<-c(rtv,aNode)
        attrs[[n]]=paste0("url(#",fid,")")
      }  
    }  
  }, list(aName=aName, aElements=aElements, aMssg=aMssg))
  ptree
}

markerEndQuote<-makeSpecTr(aName="marker-end", aElements = "marker", aMssg="Bad marker parameter")
markerMidQuote<-makeSpecTr(aName="marker-mid", aElements = "marker", aMssg="Bad marker parameter")
markerStartQuote<-makeSpecTr(aName="marker-start", aElements = "marker", aMssg="Bad marker parameter")
maskQuote<-makeSpecTr(aName="mask", aElements = "mask", aMssg="Bad mask")
clipPathQuote<-makeSpecTr(aName="clip-path", aElements = "clipPath", aMssg="Bad clipPath parameter")

# Done
# - filter: filter = filter
# - pattern: fill="url(#TrianglePattern)"  
# - radialGradient fill=linearGradient
# - linearGradient:  fill="url(#MyGradient)"
# - marker :  marker-end="url(#Triangle)" 
# - marker :  marker-start="url(#Triangle)" 
# - marker :  marker-mid="url(#Triangle)" 
