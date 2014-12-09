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
fread("dataTables/comboParams.tsv")->CO.DT
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

#1.attr is an uncombined attr
#2. loc is loc for attr
#3. variable is combined attr
co.loc<-function(attr,loc, variable){
  pattern<-paste0(attr[1],"Attribute$")
  variable<-paste0(toupper(variable[1]),'Attribute')
  sub(pattern, variable, loc, ignore.case=T)
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


# Splits vector of elements into list of categories with both 
# list names (category) and members( elements for each cat) sorted
# 
#  used by: elements.by.category.listing)
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


# uses: extract.CatMember.List
# is used by: generate.element.pages::addElementEntry, generate.Pres.Attr.Pages 
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
      # 1. get the combined attrs from CO.DT
      CO.DT[element==elName, .SD[1,], by=variable]->tmp1.DT      
      # 2. extract from AL.DT, the locations and form CAL.CO.DT for combined
      if(nrow(tmp1.DT)>0){
        setkey(tmp1.DT,value)
        #In one step :)
        CAL.CO.DT<-AL.DT[tmp1.DT,list(category='combining attributes', attr=variable, loc=co.loc2(attr, loc, variable))]
        setkey(CAL.CO.DT, attr) #make sure that it's sorted
        CAL.DT<-rbind(CAL.DT,CAL.CO.DT)        
      }
      
      #------presentation attributes
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

# requires es.DT, AVEL.DT, AVD.DT,
do.documentation<-function(es.DT, composerFiles="composerFiles"){ 
  source("genAttrDocPages.R")
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

  regAttrDocPages<-generate.Reg.Attr.Pages()
  cat(regAttrDocPages, file=paste(composerFiles, "regAttr.pages.doc.R", sep="/") )

  presAttrDocPages<-generate.Pres.Attr.Pages()
  cat(presAttrDocPages, file=paste(composerFiles, "presAttr.pages.doc.R", sep="/") )


  combAttrDocPages<-generate.CO.Attr.Pages()
  cat(combAttrDocPages, file=paste(composerFiles, "combAttr.pages.doc.R", sep="/") )
#attr doc
  #attrDefDoc<-get.Attr.defs(es.DT)
  #cat(attrDefDoc, file=paste(composerFiles, "attrDefDoc.R", sep="/") ) 
}

#do.documentation(es.DT)
