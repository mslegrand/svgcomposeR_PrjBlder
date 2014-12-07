# good read: http://www.quantumvibe.com/strip?page=1

#ghost docs
#todo!!! 
# add cxy, ... and other custom attributes (i.e for gradient)
# add alias for . and - and :  elements
# add alias for . and - (and :? ) attributes
# add description
# add meaningful titles
# rename element categories (ends and refererences ending with :)
# add categories for attributes (such as presentation, ...)
# implement the name completions in .onload
# add a main category page
# add example docs
# check if we still need an alias for add attribute entry
# compare avel with attr


# todo!!! 
# save docs to link to svgComposer
# save source to link to svgComposer
# use svgComposer in svgShiny
# write examples!!!

#testing 123

#buildDocumentation
library(data.table)
#fread("./dataTables/elementSummary.csv")->es.DT #triples: element, type, value
fread("./dataTables/elementAttrCategorySummary.tsv")->eaCS.DT
fread("dataTables/presentationAttr.tsv")->PA.DT
#------------------------ATTENTION!!!!-----------------------------------------
# tmp kludge to remove the presentation attrs
#------------------------BEGIN KLUDGE!!!!-----------------------------------------
eaCS.DT[name!="presentation attributes"]->eaCS.DT
#rbind(eaCS.DT, data.table(name="presentation attributes", value="alignment-baseline"))
#------------------------END KLUDGE!!!!-----------------------------------------


# eCS.DT<-eaCS.DT[grep("elements$", eaCS.DT$name)]
# aCS.DT<-eaCS.DT[grep("attributes$", eaCS.DT$name)]



capitalizeIt<-function(name){
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", name, perl=TRUE)
}



# ------------------NOT USING BELOW ANYMORE!!!
# generates an alphebetical Index of All Elements 
gen.all.Elem.Index<-function(es.DT){
  unique(es.DT$element)->ele
  sort(gsub("-",".", ele))->ele
  ele<-paste(" \\item \\code{\\link[svgcomposeR]{", ele, "}}", sep="")
  txt<-c(
    paste("\\itemize{"),
    ele,
    "}",
    paste("@name",  "All Elements Index- Alhabetically"),
    paste("@title", "An Alphabetical Index of All Elements")
  )
  tmp<-paste("#' ", txt, sep="", collapse="\n")  
  tmp<-paste(tmp,"\nNULL\n") 
}

#returns doc listing elements for each category
get.Elem.categories<-function(es.DT){
  addEleCategoryEntry<-function(name, elemArgs, description="", visible=TRUE ){
    txt<-c(
      paste0("@name ", gsub(' ','',name),"s"), #blue
      paste0("@title ", name,"s"), #!!!todo add something
      paste("@description ", description),
      paste("\\itemize{"),
      paste(" \\item \\code{\\link{", elemArgs, "}}", sep=""),
      "}",
      "@keywords internal"
    )
    tmp<-paste("#' ", txt, sep="", collapse="\n")
  }  
  es.DT[variable=="category", list(list(I(.I))), by=value]->tmp
  tmp<-tmp[order(value)]
  catsL<-sapply(1:nrow(tmp), 
                function(i){ addEleCategoryEntry(name=tmp$value[i], elemArgs= es.DT$element[ tmp$V1[[i]] ] )}
  )
  rtv<-paste(catsL, "\nNULL\n", collapse="\n")
}

# ------------------NOT USING ABOVE ANYMORE!!! 


# Generates an Listing of Element Categories (Index of Ele Cats)
generate.ele.cat.Index<-function(){
  # -------------BEGIN HELPERS
  oneCatListing<-function(category){
    #category name
    es.DT[variable=="category" & value==category]$element->ele    
    sort(gsub("-",".", ele))->ele # convert - to . in element names
    res<-c(
      paste0("@section ",category,"s:"),
      "\\describe{",
      paste0("\\item{ \\code{\\link[svgcomposeR]{",ele,"}}}{}"),
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


# This takes a vector of  element members and split it 
# into a list by category 
#  (used by elements.by.category.listing)
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
      tmp.DT<-rbind(tmp.DT, data.table(name=other, value=missing))
    } 
    tmp.list<-structure(lapply(cats, function(kit)tmp.DT[name==kit]$value) ,
                        names=cats)
    #tmp.list<-sort(gsub("[-:]",".", tmp.list)) # convert - to . in values
  } 
  tmp.list
}

elements.by.category.listing<-function( elemArgs ){
  elemCats<-extract.CatMember.List(elemArgs, other="Unclassfied:") 
  elemCats<-lapply(elemCats, function(x) gsub("[-:]",".", x))
  elemCats<-lapply(elemCats, function(x)paste0("\\code{\\link{",x,"}}" ) )
  
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
  gsub("[-:]",".",presAttrs)->presAttrs #remove the uglies
  presAttrsLoc<-paste0("presAttrs.", presAttrs)
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
  # expand element categoris (to get content.elements)
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

  #returns attr-link-items of all reg attrs, given an elements name
  makeAttrLinkItems2<-function(elName){
      #regular attributes
      AL.DT<- AVEL.DT[element==elName, list(loc), key=attr]
      #setkey(AL.DT, attr)
      setkey(eaCS.DT, value) #do just once please
      CAL.DT<-eaCS.DT[AL.DT]
      setnames(CAL.DT, c("category", "attr", "loc"))
      if(nrow(CAL.DT)>0){
        CAL.DT[is.na(category), category:='unclassified']
        CAL.DT[, attr:=gsub("[-:]", ".", attr)]
      }     
     
      #presentation attributes
      presAttrs<-PA.DT[variable=="Applies to" & value==elName]$attr
      if(length(presAttrs)>0){
        gsub("[-:]",".",presAttrs)->presAttrs #remove the uglies
        presAttrsLoc<-getPresAttrsLoc(presAttrs) #paste0("presAttrs.", presAttrs)      
        CAL.DT<-rbind(
          CAL.DT,
          data.table(category="presentation attributes", attr=presAttrs, loc=presAttrsLoc)
        )
      }
# tmp<-data.table(name=c("a","b"), x=1:6, y=7:12)
# split(tmp[,paste("x=",x,"y=",y)], tmp$name)
      if(nrow(CAL.DT)>0){
        setkey(CAL.DT, category, attr)
        CAL.LIST<-split(CAL.DT[,paste0("\\link[=", loc,"]{",attr,"}")] , CAL.DT$category)
        
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
    elemArgs<-expand.arg.names(elemArgs) #expands el-categories in content.elements
    # break up  elements back into el-categoris, but now is list

    elemArgsItems<- elements.by.category.listing(elemArgs)
    
    #elemArgsItems<-paste0("\\item{ \\code{\\link{", elemArgs, "}}}")
    #---end content content.element handeling------------------------------------    
    #---begin attribute  handeling---------------------------------------      
    attrArgsItems<-makeAttrLinkItems2(elName)   
    #---end content element handeling------------------------------------    
    name<-gsub("[-:]",".",elName)
    
    #pulling it together 
    description="Need to be written!!!"
    txt<-c(
      paste("@name", name), #
      paste("@title", name), #todo!!! add something meaningfull??
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

#needs 1. loc (becomes @Name)
#      2. AttrName = @title
#      3. Attr Values
#      4. Attr Values Des
#      5. Elements which attr applies to
#      6. Animatable??
#      7. related attrs??
#     
generate.Reg.Attr.Pages<-function(){
  #requries AVD.DT, AVE.DT
  addAttributeEntry<-function(alink){
    #showMe(alink)
    tmp1.DT<-AVEL.DT[loc==alink]
    elements<-tmp1.DT$element
    anim<-unique(tmp.DT$anim) #works since there is at most 1
    tmp2.DT<-AVD.DT[loc==alink]
    values<-AVD.DT[loc==alink]$value
    valDes<-AVD.DT[loc==alink]$value.def
    #showMe(valDes)
    #valDes<-gsub( "(@[-\\w:]+)" ,"\\1 attribute", valDes, perl=T)
    valDes<-gsub('[-:]',".",valDes)
    valDes<-gsub('@','',valDes)
    
    title<-unique(AVEL.DT[loc==alink]$attr )
    elements<-paste("\\code{\\link{", elements, "}}", sep="", collapse=", ")
    txt<-c(
      paste("@name", alink),
      paste("@title",title), 
      paste("@section Available Attribute Values:"),     
      paste("\\describe{"),
      paste("\\item{ ",   values, "}{", valDes,"}", sep=""),
      "}",
      paste("@section Used by the Elements:"),           
      paste("\\itemize{"),
      paste("\\item{ ",   elements, "}", sep=""),
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

generate.Pres.Attr.Pages<-function(){
  #requries PA.DT
  addAttributeEntry<-function(attribute){ #
    #showMe(alink)
    
    tmp1.DT<-PA.DT[attr==attribute]
    AppliesTo.elements<-tmp1.DT[variable=="Applies to"]$value
   
    #expand the categegories
    #AppliesTo.elements<-expand.arg.names(AppliesTo.elements)
    
    
    #process elemArgs
    #showMe(elName)
        
    elemArgsItems<- elements.by.category.listing(AppliesTo.elements)
    
    Animatable<-tmp1.DT[variable=="Animatable"]$value 
    Initial<-tmp1.DT[variable=="Initial"]$value
    Inherited<-tmp1.DT[variable=="Inherited"]$value
    values<-tmp1.DT[variable=="Value"]$value
    Percentages<-tmp1.DT[variable=="Percentages"]$value
    #showMe(valDes)
    #valDes<-gsub( "(@[-\\w:]+)" ,"\\1 attribute", valDes, perl=T)
#     valDes<-gsub('[-:]',".",valDes)
#     valDes<-gsub('@','',valDes)
    valDes<-" "
    title<-gsub("[-:]", ".", attribute) 
    presAttrLoc<-getPresAttrsLoc(title)
    # AppliesTo.elements<-paste("\\code{\\link{", AppliesTo.elements, "}}", sep="", collapse=", ")
    txt<-c(
      paste("@name", presAttrLoc),
      paste("@title",title), 
      paste("@section Available Attribute Values:"),     
      paste("\\itemize{"), #paste("\\describe{"),
      paste("\\item{ ",   values, "}{", valDes,"}", sep=""),
      "}",
      paste("@section Used by the Elements:"), 
      "\\describe{",
      elemArgsItems,
      "}",
#        paste("\\itemize{"),
#        paste("\\item{ ",   AppliesTo.elements, "}", sep=""),
#        "}",
      "@keywords internal"
    )
    tmp<-paste("#' ", txt, sep="", collapse="\n")  
  }

  attrs<-unique(PA.DT[variable=="Applies to"]$attr)
  #attrs[-grep("@",attrs)]->links #kludge to accomadate some bad data
  #links<-paste0("presAttrs.", attrs) 


  attrDefsPages.List<-lapply( attrs, addAttributeEntry)
  
  rtv<-paste(attrDefsPages.List, "\nNULL\n", collapse="\n")
  rtv 
}


# requires es.DT, AVEL.DT, AVD.DT,
do.documentation<-function(es.DT, composerFiles="composerFiles"){ 
#listing of all elements
#   eleAlphabeticalIndexDoc<-gen.all.Elem.Index(es.DT) 
#   cat(eleAlphabeticalIndexDoc, file=paste(composerFiles, "eleAlphabeticalIndexDoc.R", sep="/"))
  
  #listing of Element by Categories
  ele.cat.indx<-generate.ele.cat.Index()
  cat( ele.cat.indx, file=paste(composerFiles, " ele.cat.indx.page.R", sep="/") )
  
  #elecat doc
#   elemCatDoc<-get.Elem.categories(es.DT)
#   cat(elemCatDoc, file=paste(composerFiles, "elemCatDoc.R", sep="/") )
  
  #individual element documentation
  ele.pages<-generate.element.pages()
  cat(ele.pages, file=paste(composerFiles, "ele.pages.doc.R", sep="/") )

  regAttrPagesDoc<-generate.Reg.Attr.Pages()
  cat(regAttrPagesDoc, file=paste(composerFiles, "regAttr.pages.doc.R", sep="/") )

  presAttrPagesDoc<-generate.Pres.Attr.Pages()
  cat(presAttrPagesDoc, file=paste(composerFiles, "presAttr.pages.doc.R", sep="/") )

  #attr doc
  #attrDefDoc<-get.Attr.defs(es.DT)
  #cat(attrDefDoc, file=paste(composerFiles, "attrDefDoc.R", sep="/") ) 
}

#do.documentation(es.DT)
