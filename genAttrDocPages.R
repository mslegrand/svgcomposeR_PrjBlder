
#needs 1. loc (becomes @Name)
#      2. AttrName = @title
#      3. Attr Values
#      4. Attr Values Des
#      5. Elements which attr applies to
#      6. Animatable??
#      7. related attrs??
# 


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


#----Regular Attribute Pages------

# used only in generate.Reg.Attr.Pages
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

requireTable(AVD.DT, AVEL.DT)


generate.Reg.Attr.Pages<-function(){
  #requries AVD.DT, AVEL.DT
  addAttributeEntry<-function(alink){
    #showMe(alink)
    #AVD.DT[loc=='KeyTimesAttribute']
    AVD.DT[loc=='KeyTimesAttribute']$value.def->txt
    strsplit(txt,"In particular, see")[[1]]->stxt
    txt<-stxt[1]
    AVD.DT[loc=='KeyTimesAttribute', value.def:=txt]
    
    
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
    if(alink=="KeyTimesAttribute"){
      cat("\n**KeyTimesAttribute**\n")
      #values="xx"
      valDes="yy"
    }
    elemArgsItems<- elements.by.category.listing(elements)

    
    txt<-c(
      rd.name(alink),
      rd.title(asDot(title)),
      rd.description("ToDo: Needs to be written!!!"),
      if(length(values)>0){
        c(rd.section("Available Attribute Values"),     
        rd.describe( rd.item(values,valDes) ) )
      } else {
        cat("WARNING",alink, ": missing attribute values\n")
        NULL
      },   
      rd.section("Used by the Elements"), 
      rd.describe( elemArgsItems),
      rd.keywords("internal") 
    )
    tmp<-paste0(rd.close(txt) , collapse="\n" )
    tmp
  }


  links<-unique(AVEL.DT$loc)
  attrDefsPages.List<-lapply( links, addAttributeEntry)
  
  rtv<-paste(attrDefsPages.List,  collapse="\n")
  rtv 
}

#--------Combo Attribute Pages----------------------


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

    
    valueN<-paste0("value.",toupper(component))
    equivI<-paste0(title, "=c(",paste(valueN, collapse=","), ")")
    equivII<-paste(component,"=",valueN, collapse="; ")

    txt<-c(
      rd.name(alink),
      rd.title(asDot(title)),
      rd.description("ToDo: Needs to be written!!!"),
      rd.section("Combines"),     
      rd.comma(nameWithLink(component, component.loc)),
      rd.section("Equivalence"),
      rd.describe( 
        c( rd.item("", asDot(equivI)),
            rd.item("and", asDot(equivII))
        )
      ),
      rd.section("Used by the Elements"), 
      rd.describe( elemArgsItems ),
      rd.keywords("internal")
    )
    tmp<-paste0(rd.close(txt) , collapse="\n" )
    tmp
  } #end addAttributeEntry
  
  #for each location, get the subtable, and process
  links<-unique(COLCL.DT$loc)
  attr.Pages.List<-lapply( links, addAttributeEntry)  
  rtv<-paste(attr.Pages.List,  collapse="\n")
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
    
    valDes<-"**ToDo!!!** "
    #title<-gsub("[-:]", ".", attribute) 
    presAttrLoc<-getPresAttrsLoc(attribute)
    # AppliesTo.elements<-paste("\\code{\\link{", AppliesTo.elements, "}}", sep="", collapse=", ")
    txt<-c(
      rd.name(presAttrLoc),
      rd.title(asDot(attribute)),
      rd.description("ToDo: Needs to be written!!!"),
      rd.section("Available Attribute Values"),     
      rd.itemize( rd.item(values, valDes)),
      rd.section("Used by the Elements"),
      rd.describe( elemArgsItems),
      rd.keywords("internal")
    )
    tmp<-paste0(rd.close(txt) , collapse="\n" )
    tmp 
  }
      
  attrs<-unique(PA.DT[variable=="Applies to"]$attr)
  attrDefsPages.List<-lapply( attrs, addAttributeEntry)  
  rtv<-paste(attrDefsPages.List, collapse="\n")
  rtv 
}
