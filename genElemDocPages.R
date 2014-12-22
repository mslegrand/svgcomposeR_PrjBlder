
# Generates an Listing of Element Categories (Index of Ele Cats)
generate.ele.cat.Index<-function(){
  # -------------BEGIN HELPERS
  oneCatListing<-function(category){
    #category name
    es.DT[variable=="category" & value==category]$element->ele    
    #sort(gsub("-",".", ele))->ele # convert - to . in element names
    sort(ele)->ele
    res<-c(
      paste0("@section ",category,"s:"),
      "\\describe{",
      paste0("\\item{", nameWithLink(ele), "}{}"),
      #"}",
      "}"
    )
    paste(res, collapse="\n#' ")    
  } 
  # -------------end HELPERS  
  #categories identified by es.DT
  cats<-unique(es.DT[variable=="category"]$value)
  cats<-sort(cats)
  # Element Group Name
  sapply(cats, oneCatListing)->cat.index
  c(
    cat.index,
    "@name Element Index",
    "@title Element Generators Indexed by Category",  
    "@description This is a listing by category of generators to use when generating an svg markup."
  )->cat.index 
  paste(cat.index, collapse="\n#' ")->cat.index 
  paste0("#' ",cat.index, "\nNULL\n")
}


# Splits vector of elements into list of categories with both 
# list names (category) and members( elements for each cat) sorted
# 
#  USED ONLY BY: elements.by.category.listing
extract.CatMember.List<-function(members, other="Unclassifed"){
  #expand??
  #other<-"Other"
  if(length(members)==0){
    tmp.list<-list()
  } else {
    rowNum<-match(members, eaCS.DT$value, nomatch=0L)
    missing<-members[rowNum==0]
    tmp.DT<-eaCS.DT[value %in% members]
    cats<-sort(unique(tmp.DT$name))
    if(length(missing)>0){
      cats<-c(cats,other)
      tmp.DT<-data.table(rbind(tmp.DT, data.table(name=other, value=missing)))
    } 
    tmp.list<-structure(lapply(cats, function(kit)tmp.DT[name==kit]$value) ,
                        names=cats)
    #tmp.list<-sort(gsub("[-:]",".", tmp.list)) # convert - to . in values
  } 
  tmp.list
}


# uses: extract.CatMember.List
# is used by: 
# generate.element.pages::addElementEntry, 
# generate.Pres.Attr.Pages 
elements.by.category.listing<-function( elemArgs ){
  elemCats<-extract.CatMember.List(elemArgs, other="Unclassfied:")   
  #elemCats<-lapply(elemCats, function(x) gsub("[-:]",".", x))  
#   elementsRD<-function(elements){
#     if(any(grepl('Empty', elements))|
#          any(grepl('Any element',elements))){
#       paste("\\code{", elements, "}", sep="", collapse=", ")
#     } else {
#       nwl<-nameWithLink(elements)
#       paste(nwl, sep="", collapse=", ")
#     }  
#   }
    
  elemCats<-lapply(elemCats, function(x){
    if(any(grepl('Empty', x))| any(grepl('Any element',x))){
      paste0("\\code{",x,"}")
    } else {
      #paste0("\\code{\\link{",x,"}}")
      nameWithLink(x)
    }
  })
  
  elemArgsItems<-lapply(names(elemCats),function(category){
    paste(
      "\\item{\\emph{",
      capitalizeIt(category),
      "}}{",
      paste(elemCats[[category]],collapse=", "),
      "}",
      sep="",
      collapse=", "
    )
  })
  unlist(elemArgsItems)->elemArgsItems   
}


# convert presAttr name into a location reference
getPresAttrsLoc<-function(presAttrs){
  #gsub("[-:]",".",presAttrs)->presAttrs #remove the uglies
  presAttrsLoc<-paste0(presAttrs,"-presentationAttribute")
  presAttrsLoc
}


#generates element documentation  for each element found in es.DT
# resulting for each element
# element name, content.model, attributes
generate.element.pages<-function(){
  
  # ---------BEGIN HELPERS:generate.element.pages
  
  #todo replace reference to eaCS.DT: 
  # with AVEL.DT and PA.DT
  # done: replace for attrs, but still used to
  # expand element categories (to get content.elements)
  expand.arg.names<-function(arg.names){
    fn<-function(x){
      if(grepl(":$",x)){
        xx<-gsub(":$","",x)
        #x<-eaCS.DT[name==xx]$value
        x<-eaCS.DT[name==xx]$value
      } else {
        x
      }    
    }    
    unlist(lapply(arg.names, fn))   
  }
  
  expand.content.ele.names<-function(arg.names){
    arg.names<-gsub(":$","",arg.names)
    names<-unique(eaCS.DT$name)
    ele.cat.names<-names[grep("elements$",names)]
    indx.cat<-match(arg.names, ele.cat.names, nomatch=0L)
    arg.ele.no.cat<-arg.names[indx.cat==0]
    arg.ele.cat<-arg.names[indx.cat>0]
    arg.ele.cat<-lapply(arg.ele.cat, function(x)sort(eaCS.DT[name==x]$value) )
    c(unlist(arg.ele.cat),arg.ele.no.cat)
  }
  
  
  # convert a vector of elements into a list index by category
  # todo!!! rewrite ele.by.cat.list to replace extract.CatMember.List
  ele.by.cat.list<-function(elements, other="Unclassified"){
    setkey(eaCS.DT,name) #should do only once!
    if(length(elements)==0){
      tmp.list<-list()
    } else {
      rowNum<-match(elements, eaCS.DT$value, nomatch=0L)
      cats<-rep("ZZZ",length(elements))
      cats[rowNum>0]<-eaCS.DT$name[rowNum>0]
      tmp.list<-split(elements,cats)
    } 
    tmp.list
  }
  
  
  #--------
  
  #returns attr-link-items of all  attrs, given an elements name
  makeAttrLinkItems2<-function(elName){
    #------regular attributes
    AL.DT<- AVEL.DT[element==elName, list(loc), key=attr]
    #setkey(AL.DT, attr)
    setkey(eaCS.DT, value) #do just once please!!!
    CAL.DT<-eaCS.DT[AL.DT]
    
    setnames(CAL.DT, c("category", "attr", "loc"))
    if(nrow(CAL.DT)>0){
      CAL.DT[is.na(category), category:='unclassified']
      CAL.DT[, attr:=gsub("[-:]", ".", attr)]
    } 
    
    co.loc2<-function(attr,loc, variable){
      sapply(1:length(attr), function(i){
        pattern<-paste0(attr[i],"Attribute$")
        variable<-paste0(toupper(variable[i]),'Attribute')
        sub(pattern, variable, loc[i], ignore.case=T)    
      }
      )   
    }
    
    #-----combo attributes
    # 1. get the combined attrs from COP.DT
    COP.DT[element==elName, .SD[1,], by=variable]->tmp1.DT      
    # 2. extract from AL.DT, the locations and form CAL.COP.DT for combined
    if(nrow(tmp1.DT)>0){
      setkey(tmp1.DT,value)
      #In one step :)
      CAL.COP.DT<-AL.DT[tmp1.DT,list(category='combining attributes', attr=variable, loc=co.loc2(attr, loc, variable))]
      setkey(CAL.COP.DT, attr) #make sure that it's sorted
      CAL.DT<-data.table(rbind(CAL.DT,CAL.COP.DT))        
    }
    
    #------presentation attributes
    presAttrs<-PA.DT[variable=="Applies to" & value==elName]$attr
    if(length(presAttrs)>0){
      #gsub("[-:]",".",presAttrs)->presAttrs #remove the uglies
      presAttrsLoc<-getPresAttrsLoc(presAttrs) #paste0("presAttrs.", presAttrs)      
      CAL.DT<-data.table(rbind(
        CAL.DT,
        data.table(category="presentation attributes", attr=presAttrs, loc=presAttrsLoc)
      ))  
    }
    # tmp<-data.table(name=c("a","b"), x=1:6, y=7:12)
    # split(tmp[,paste("x=",x,"y=",y)], tmp$name)
    if(nrow(CAL.DT)>0){
      setkey(CAL.DT, category, attr)
      CAL.LIST<-split(CAL.DT[, nameWithLink(attr, loc)
                            #paste0("\\link[=", loc,"]{",attr,"}")
                            ] , CAL.DT$category)
      
      fn<-function(cat.name){
        paste(
          "\\item{\\emph{",
          capitalizeIt(cat.name),
          "}}{",
          paste(CAL.LIST[[cat.name]], collapse=", "),
          "}",
          sep="",
          collapse=", "
        )
      }        
      attributesListing<-unlist(lapply(names(CAL.LIST), fn ))
    } else {
      attributesListing<-"{No Attributes Available}{!}"
    }
    attributesListing
  }
  
  #helper fn to write doc for single element
  addElementEntry<-function(elName){
    #showMe(elName)
    # begin---content.element handeling--------------
    elemArgs<-es.DT$value[ content.DT[element==elName]$content[[1]] ]
    #elemArgs<-expand.arg.names(elemArgs) #expands el-categories in content.elements
    elemArgs<-expand.content.ele.names(elemArgs)
    # break up  elements back into el-categoris, but now is list
    
    elemArgsItems<- elements.by.category.listing(elemArgs)
    
    #elemArgsItems<-paste0("\\item{ \\code{\\link{", elemArgs, "}}}")
    #---end content content.element handeling------------------------------------    
    #---begin attribute  handeling---------------------------------------      
    attrArgsItems<-makeAttrLinkItems2(elName)   
    #---end content element handeling------------------------------------    
    #name<-gsub("[-:]",".",elName)
    
    #pulling it together 
    description="Need to be written!!!"
    txt<-c(
      paste("@name", elName ), #
      paste("@title", asDot(elName)), #
      "@description ",
      description,    
      "@section Available Attributes (Named Parameters):",
      "\\describe{",
      attrArgsItems,
      "}",
      "@section Available Content Elements (Unnamed Parameters):",
      "\\describe{",
      elemArgsItems,
      "}",
      
      "@keywords internal"
    )
    tmp<-paste("#' ", txt, sep="", collapse="\n")
    tmp
  } #END: addElementEntry
  
  # ---------END HELPERS:generate.element.pages
  
  #vector of all elements
  unique(es.DT$element)->all.elements
  # content.DT
  es.DT[variable=="content.model", list(content=list(I(.I))), by=element]->content.DT
  es.DT[variable=="attr",  list(attr=list(I(.I))), by=element]->attributes.DT
  
  eleL<-lapply( all.elements, addElementEntry)
  rtv<-paste(eleL, "\nNULL\n", collapse="\n")
  
} 
#----------- END: generate.element.pages
