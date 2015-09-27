

#special tag handling

# in.defs.only.elements<-c("clipPath", "cursor", "filter", "linearGradient", "marker", "mask", "pattern", "radialGradient", "symbol")

#place after promoteUnamedLists
animateTagQuote<-quote(
{
  combos<-list(cxy = c('cx', 'cy'), dxy = c('dx', 'dy'), 
               fxy = c('fx', 'fy'), g12 = c('g1', 'g2'), 
               'horiz-origin-xy' = c('horiz-origin-x', 
                                     'horiz-origin-y'), 
               in12 = c('in', 'in2'), k1234 = c('k1', 'k2', 'k3', 'k4'), 
               pointsAtXYZ = c('pointsAtX', 'pointsAtY', 'pointsAtZ'), 
               refXY = c('refX', 'refY'), rxy = c('rx', 'ry'), 
               targetXY = c('targetX', 'targetY'), u12 = c('u1', 'u2'), 
               'vert-origin-xy' = c('vert-origin-x', 'vert-origin-y'), 
               wh = c('width', 'height'), x12 = c('x1', 'x2'), xy = c('x', 'y'), 
               xy1 = c('x1', 'y1'), xy2 = c('x2', 'y2'),     xyz = c('x', 'y', 'z'), 
               y12 = c('y1', 'y2'))
  attributeName<-args[["attributeName"]]
  if( !is.null(attributeName) && attributeName %in% names(combos) ){
    attributeNames<-combos[[attributeName]]
    N<-length(attributeNames)
    tmp<-lapply( 1:N, function(i){
      args2<-args
      args2[["attributeName"]]<-attributeNames[i]
      tmp<-c("from","to","values")
      ind<-intersect(names(args),tmp)
      args2[ind]<-lapply(args2[ind], function(vec){
        j<-min(i,length(vec))
        vec[j]
      })              
      animate(args2)
    })
    return(tmp)    
  }
}
)



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
  
  indx.in<-which(names(attrs)=='in' | names(attrs)=='in2')
  
  for(n in indx.in){
    an<-attrs[[n]]
    if (inherits(an, 'list') && length(an)>=1){ 
      len<-length(an)
      rtv<-c(rtv, an[1:(len-1)])
      feNode=an[[len]]}
    else{
      feNode=an
    }
    if(inherits(feNode, "XMLAbstractNode")){ #may want to require tag is fe!=feMergeNode??
      resultStr<-getsafeNodeAttr("result", feNode) #only if !=fe
      rtv<-c( rtv, XfeNode=feNode)
      attrs[[n]]<-resultStr            
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


fillQuote<-makeSpecTr(aName="fill", aElements = c("pattern", "linearGradient", "radialGradient"), aMssg="Bad fill parameter")
markerEndQuote<-makeSpecTr(aName="marker-end", aElements = "marker", aMssg="Bad marker parameter")
markerMidQuote<-makeSpecTr(aName="marker-mid", aElements = "marker", aMssg="Bad marker parameter")
markerStartQuote<-makeSpecTr(aName="marker-start", aElements = "marker", aMssg="Bad marker parameter")
maskQuote<-makeSpecTr(aName="mask", aElements = "mask", aMssg="Bad mask")
clipPathQuote<-makeSpecTr(aName="clip-path", aElements = "clipPath", aMssg="Bad clipPath parameter")

makeAni<-function(etag, aaCombos){
  #etag is the tag element tag (animate, set)
  #aaCombos is all the animate combos
  c(
    substitute(combos<-aaCombos, list(aaCombos=aaCombos )),
    quote(attributeName<-args[["attributeName"]]),
    substitute(
      if( !is.null(attributeName) && attributeName %in% names(combos) ){
        attributeNames<-combos[[attributeName]]
        N<-length(attributeNames)
        tmp<-lapply( 1:N, function(i){
          args2<-args
          args2[["attributeName"]]<-attributeNames[i]
          tmp<-c("from","to","values")
          ind<-intersect(names(args),tmp)
          args2[ind]<-lapply(args2[ind], function(vec){
            j<-min(i,length(vec))
            vec[j]
          })              
          etag(args2)
        })
        return(tmp)    
      },
      list(etag=etag, aaCombos=aaCombos)
    )  
  )
}



