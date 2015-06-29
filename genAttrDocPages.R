

cleanAttrValue<-function(vd){
  vd<-gsub('[-:]',".",vd)
  vd<-gsub('[‘’]',"'",vd)  
  vd<-gsub('[@]','',vd)
  vd<-gsub('…','...', vd)
  #valDes<-gsub("[@‘’ é…−™]","",valDes)
  vd<-gsub("[@‘’é…−™]","",vd)
  iconv(vd, "latin1", "ASCII", sub="")
  vd
}

#' not used, but can replace 
#' elements.by.category.listing to  get element listing
#' as a single comma seperated list
elementsRD<-function(elements){
  sort(elements)->elements
  if(any(grepl('Empty', elements))|
     any(grepl('Any element',elements))){
    paste("\\code{", elements, "}", sep="", collapse=", ")
  } else {
    paste( nameWithLink(elements), sep="", collapse=", ")
  }  
}

#needs 1. loc (becomes @Name)
#      2. AttrName = @title
#      3. Attr Values
#      4. Attr Values Des
#      5. Elements which attr applies to
#      6. Animatable??
#      7. related attrs??
#     
generate.Reg.Attr.Pages<-function(){
  #requries AVD.DT, AVEL.DT
  addAttributeEntry<-function(alink){
    #showMe(alink)
    tmp1.DT<-AVEL.DT[loc==alink]
    elements<-tmp1.DT$element
    #elements<-gsub("-",".",elements)
    anim<-unique(tmp1.DT$anim) #works since there is at most 1
    tmp2.DT<-AVD.DT[loc==alink]
    values<-AVD.DT[loc==alink]$value
    valDes<-AVD.DT[loc==alink]$value.def
    values<-cleanAttrValue(values)
    valDes<-cleanAttrValue(valDes)
    title<-unique(AVEL.DT[loc==alink]$attr )
    elemArgsItems<- elements.by.category.listing(elements)

    txt<-c(
      paste("@name", alink),
      paste("@title",asDot(title)), 
      paste("@section Available Attribute Values:"),     
      paste("\\describe{"),
      paste("\\item{ ",   values, "}{", valDes,"}", sep=""),
      "}",
      paste("@section Used by the Elements:"), 
      "\\describe{",
      elemArgsItems,
      "}",
      "@keywords internal"
      )
    tmp<-paste("#' ", txt, sep="", collapse="\n")  
  }
  links<-unique(AVEL.DT$loc)
  attrDefsPages.List<-lapply( links, addAttributeEntry)
  
  rtv<-paste(attrDefsPages.List, "\nNULL\n", collapse="\n")
  rtv 
}


generate.CO.Attr.Pages<-function(){
  #helper fn
  co.loc2<-function(attr,loc, variable){
    sapply(1:length(attr), function(i){
      pattern<-paste0(attr[i],"Attribute$")
      pattern<-gsub("-","",pattern)
      variable<-paste0(toupper(variable[i]),'Attribute')
      variable<-gsub("-","",variable)
      sub(pattern, variable, loc[i], ignore.case=T)    
    }
    )   
  }
    
  #1. get the COLCL.DT data
  AL.DT<- AVEL.DT[, list(element, attr, loc)]
  
  setkey(COP.DT,element,value)
  setkey(AL.DT,element,attr)
  AL.DT[COP.DT, list(element=element, attr=variable, component=value, component.loc=loc)]->COCL.DT
  
  COP.DT[,.SD[1,],by=list(element,variable)]->tmp1.DT
  setkey(tmp1.DT,element,value)
  #1.attr is an uncombined attr
  #2. loc is loc for attr
  #3. variable is combined attr
  AL.DT[tmp1.DT, list(element=element, attr=variable, loc=co.loc2(attr, loc, variable))]->COL.DT
  
  merge(COL.DT,COCL.DT, by=c("element", "attr"))->COLCL.DT
  
  addAttributeEntry<-function(alink){
    tmp1.DT<-COLCL.DT[loc==alink]
    elements<-tmp1.DT$element
    title<-unique(tmp1.DT$attr)
    component<-unique(tmp1.DT$component)
    component.loc<-unique(tmp1.DT$component.loc)
    elemArgsItems<- elements.by.category.listing(elements)

    componentWLink<-paste0("\\link[=", component.loc,"]{",component,"}")
    componentComma<-paste(componentWLink, sep="", collapse=", ")
    
    valueN<-paste0("value.",toupper(component))
    equivI<-paste0(title, "=c(",paste(valueN, collapse=","), ")")
    equivII<-paste(component,"=",valueN, collapse="; ")

    txt<-c(
      paste("@name", alink),
      paste("@title",asDot(title)), 
      paste("@section Combines:"),
      componentComma,
      "@section Equivalence:",
        "\\describe{",
          paste0("\\item{}{",   asDot(equivI), "}"),
          paste0("\\item{and}{",asDot(equivII),"}"),
        "}",  
      paste("@section Used by the Elements:"), 
        "\\describe{",
        elemArgsItems,
        "}",
        "@keywords internal"
    )
    tmp<-paste("#' ", txt, sep="", collapse="\n")  
  } #end addAttributeEntry
  
  #for each location, get the subtable, and process
  links<-unique(COLCL.DT$loc)
  attr.Pages.List<-lapply( links, addAttributeEntry)  
  rtv<-paste(attr.Pages.List, "\nNULL\n", collapse="\n")
  rtv 
}


#' Uses:
#' elements.by.category.listing
#' expand.pres.Cat
#' elements.by.category.listing
#' 
generate.Pres.Attr.Pages<-function(){
  #requries PA.DT
  addAttributeEntry<-function(attribute){ #
    #showMe(alink)
    expand.pres.Cat<-function(x){
      pec<-list(
        "shape elements"=c('path', 'rect', 'circle', 'ellipse', 'line', 'polyline', 'polygon'),
        "container elemenst"= c('a', 'defs', 'glyph', 'g', 'marker', 'mask', 'missing-glyph', 'pattern', 'svg', 'switch', 'symbol'),
        "text content element"= c('altGlyph', 'textPath', 'text', 'tref', 'tspan'),
        "image elements"=c('image')
      )
      match(x,names(pec),nomatch = 0L)->indx
      sort(c(unlist(pec[indx[indx>0]]), x[indx==0]))
    }
    
    tmp1.DT<-PA.DT[attr==attribute]
    AppliesTo.elements<-tmp1.DT[variable=="Applies to"]$value
    AppliesTo.elements<- expand.pres.Cat( AppliesTo.elements)
    elemArgsItems<- elements.by.category.listing(AppliesTo.elements)
    
    Animatable<-tmp1.DT[variable=="Animatable"]$value 
    Initial<-tmp1.DT[variable=="Initial"]$value
    Inherited<-tmp1.DT[variable=="Inherited"]$value
    values<-tmp1.DT[variable=="Value"]$value
    Percentages<-tmp1.DT[variable=="Percentages"]$value
    
    valDes<-"**to do** "
    #title<-gsub("[-:]", ".", attribute) 
    presAttrLoc<-getPresAttrsLoc(attribute)
    # AppliesTo.elements<-paste("\\code{\\link{", AppliesTo.elements, "}}", sep="", collapse=", ")
    txt<-c(
      paste("@name", presAttrLoc),
      paste("@title", asDot(attribute)), 
      paste("@section Available Attribute Values:"),     
      paste("\\itemize{"), #paste("\\describe{"),
      paste("\\item{ ",   values, "}{", valDes,"}", sep=""),
      "}",
      paste("@section Used by the Elements:"), 
      "\\describe{",
      elemArgsItems,
      "}",
      "@keywords internal"
    )
    tmp<-paste("#' ", txt, sep="", collapse="\n")  
  }
  attrs<-unique(PA.DT[variable=="Applies to"]$attr)
  attrDefsPages.List<-lapply( attrs, addAttributeEntry)  
  rtv<-paste(attrDefsPages.List, "\nNULL\n", collapse="\n")
  rtv 
}
