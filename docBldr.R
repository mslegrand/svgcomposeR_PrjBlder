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
aCS.DT<-eaCS.DT[name %like% " attributes"]

capitalizeIt<-function(name){
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", name, perl=TRUE)
}

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



addAttributeEntry<-function(name, elemArgs, description=""){
  if(length(name)>1){
    namea<-paste(name, collapse=" ")
    name<-paste(name, collapse=".")
    alias<-paste("@alias", namea) 
  } else
    alias<-NULL
  elemArgsF<-paste("\\code{\\link{", elemArgs, "}}", sep="", collapse=", ")
  txt<-c(
    paste("@name", name),
    paste("@title",name), #todo!!! replace with something meaninful
    alias,
    paste("@description ", description),
    paste("\\itemize{"),
    paste(" \\item{Used by the Elements:}{ ",   elemArgsF, "}", sep=""),
    "}",
    "@keywords internal"
  )
  tmp<-paste("#' ", txt, sep="", collapse="\n")  
}

# # Generates an Listing of Element Categories (Index of Ele Cats)
# generate.ele.cat.Index<-function(es.DT){
#   cats<-unique(es.DT[variable=="category"]$value)
#   cats<-sort(cats)
#   cats<-gsub(' ','',cats)
#   #paste("\\item{",cats,"}", sep="")
#   #cats2<-paste(" \\item{",cats,"}{ \\code{\\link[svgcomposeR]{", cats, "}}}", sep="") 
#   cats2<-paste(" \\item \\code{\\link[svgcomposeR]{", cats, "s}}", sep="")
#   txt<-c(
#     paste("\\itemize{"),
#     cats2,
#     "}",
#     paste("@name",  "All Elements Index - by Category"),
#     paste("@title", "Elements Index by Category")
#   )
#   tmp<-paste("#' ", txt, sep="", collapse="\n")  
#   tmp<-paste(tmp,"\nNULL\n")
# }

#generates an alphebetical Index of All Elements
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

# Generates an Listing of Element Categories (Index of Ele Cats)
# Generates an Listing of Element Categories (Index of Ele Cats)
generate.ele.cat.Index<-function(es.DT){
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
  cats<-unique(es.DT[variable=="category"]$value)
  cats<-sort(cats)
  # Element Group Name
  sapply(cats, oneCatListing)->cat.index
  c(
    cat.index,
    "@name SVG Element Creators",
    "@title Elements Indexed by Category"  
  )->cat.index 
  paste(cat.index, collapse="\n#' ")->cat.index 
  paste0("#' ",cat.index, "\nNULL\n")
}


#returns doc listing elements for each category
get.Elem.categories<-function(es.DT){
  es.DT[variable=="category", list(list(I(.I))), by=value]->tmp
  tmp<-tmp[order(value)]
  catsL<-sapply(1:nrow(tmp), 
    function(i){ addEleCategoryEntry(name=tmp$value[i], elemArgs= es.DT$element[ tmp$V1[[i]] ] )}
  )
  rtv<-paste(catsL, "\nNULL\n", collapse="\n")
}



#generates element documentation  for each element from es.DT
# resulting for each element
# element name, content.model, attributes
generate.element.pages<-function(es.DT){

  #begin content element handleing
  getEleInGrp<-function(category){
    category<-sub("s$","",category)
    es.DT[variable=="category" & value==category]$element->ele 
    if(length(ele)==0){
      ele<-"Empty"
    } else {
      ele<-sort(gsub("-",".", ele)) # convert - to . in element names
    }
    ele
  }
  
  getAttrInGrp<-function(category){
    #category<-sub("s$","",category)
    aCS.DT[name==category]$value->attrs
    if(length(attrs)==0){
      attrs<-"Empty"
    } else {
      attrs<-sort(gsub("[-:]",".", attrs)) # convert - to . in attr names
    }
    attrs   
  }
  
  splitIntoGrps<-function(args, fn=getAttrInGrp){
    #process rrgs
    if(length(args)==0){
      grp.list<-list( Empty= "No available content elements")
    } else {
      grpIndx<-grep(":$",args)
      specIndx<-setdiff(1:length(args), grpIndx)
      #grps<-args[grpIndx]
      spec.names<-args[specIndx]
      if(length(grpIndx)>0){ #we have some groups (i.e.  categories)
        grp.names<-args[grpIndx]
        #for elemGrps remove final colon  and captilize 
        grp.names<-sub(":$","",grp.names)       
        grp.list<-lapply(grp.names, fn) #!!!
        grp.names<-capitalizeIt(grp.names)
        names(grp.list)<-grp.names
        grp.names<-sort(grp.names)
        
        #elemGrps<-sub(" ","-",elemGrps)
      } else { #no groups
        grp.names<-c()
        grp.list<-list()
        spec.names<-args
      }     
      if(length(spec.names)>0){
        spec.names<-sort(spec.names)
        grp.list<-c(grp.list, list("Other Elements:"= spec.names))
        grp.names<-c(grp.names,"Other Elements:")
      }
    }
    
    grp.list<-lapply(grp.names, function(x){ paste0("\\code{\\link{", grp.list[[x]], "}}")})
    names(grp.list)<-grp.names
    grp.list
      #grp.list, function(x)paste0("\\code{\\link{",x,"}}" ) )    
  }
  
  
  #helper fn to write doc for single element
  addElementEntry<-function(name, elemArgs, attrArgs, description="Needs Description Written!!!"){
    #process elemArgs
    showMe(name)
    if(length(elemArgs)==0){
      elemCats<-list( Empty= "No available content elements")
    } else {
      grpIndx<-grep(":$",elemArgs)
      specIndx<-setdiff(1:length(elemArgs), grpIndx)
      elemGrps<-elemArgs[grpIndx]
      elemSpec<-elemArgs[specIndx]
      if(length(grpIndx)>0){ #we have some groups (i.e.  categories)
        elemGrps<-elemArgs[grpIndx]
        #for elemGrps remove final colon  and captilize 
        elemGrps<-sub(":$","",elemGrps)
        elemGrps<-capitalizeIt(elemGrps)
        elemCats<-lapply(elemGrps, getEleInGrp) #!!!
        names(elemCats)<-elemGrps
        elemGrps<-sort(elemGrps) #sorting the names
        #elemGrps<-sub(" ","-",elemGrps)
      } else { #no groups
        elemCats<-list()
        elemSpec<-elemArgs
      }     
      if(length(elemSpec)>0){
        elemSpec<-sort(elemSpec)
        elemCats<-c(elemCats,list("Other Elements:"= elemSpec))
      }
    }
    elemCats<-lapply(elemCats, function(x)paste0("\\code{\\link{",x,"}}" ) )
    elemArgsItems<-lapply(names(elemCats),function(category){
      paste(
        "\\subsection{",
        category,
        "} {",
        paste(elemCats[[category]],collapse=", "),
        "}",
        sep="",
        collapse=", "
      )
    })
    unlist(elemArgsItems)->elemArgsItems
    #elemArgsItems<-paste0("\\item{ \\code{\\link{", elemArgs, "}}}")
    #---end content element handlers------------------------------------
    
    #---begin attribute  handlers---------------------------------------
    attrArgs<-splitIntoGrps(args=attrArgs, fn=getAttrInGrp)
    attrArgsItems<-sapply(names(attrArgs),function(n){
      paste0("\\item{ ", n,"}{", paste(attrArgs[[n]],collapse=", "), "}")
    })
    #attrArgsItems<-paste0("\\item{ ", attrArgs, "}")  
    # if ends with colon is group 
    # to find attrs in group must remove colon
    # 1. split into group and specific
    # 2. populate grp 
    # 3. populate spec
    # 4. reformat attrs having - and colons
    # 5. add custom attrs
    # 6. 
    
    #---begin attribute  handlers---------------------------------------
    # if ends with a colon, find the attribute class
    
    txt<-c(
      paste("@name", name),
      paste("@title", name), #todo!!! add something
      "@description ",
      description,
      "@section Available Content Elements:",
      #"\\describe{",
      elemArgsItems,
      #"}",
      "@section Available Attributes:",
      "\\describe{",
      attrArgsItems,
      "}",
      "@keywords internal"
    )
    tmp<-paste("#' ", txt, sep="", collapse="\n")
    tmp
  }
    


#   #helper fn to write doc for single element
#   addElementEntry<-function(name, elemArgs, attrArgs, description="Needs Description Written!!!"){
#     grpIndx<-grep(":$",elemArgs)
#     if(length(grpIndx)>0){ #we have some groups (i.e.  categories)
#       elemGrps<-elemArgs[grpIndx]
#       elemSpec<-elemArgs[-grpIndx]
#       #for elemGrps remove final colon  and captilize 
#       elemGrps<-sub(":$","",elemGrps)
#       elemGrps<-capitalizeIt(elemGrps)
#       elemCats<-lapply(elemGrps, getEleInCat)
#       #elemGrps<-sub(" ","-",elemGrps)
#     } else { #no groups
#       elemGrps<-NULL
#       elemSpec<-elemArgs
#     }
#     if(!is.null(elemGrps)){
#       elemGrps<-sort(elemGrps)
#     }
#     if(!is.null(elemSpec)){
#       elemSpec<-sort(elemSpec)
#     }
#     elemArgs<-c(elemGrps,elemSpec)
#     if(length(elemArgs)==0){
#       elemArgs="Empty"
#     }
#     elemArgsItems<-paste0("\\item{ \\code{\\link{", elemArgs, "}}}")
#     attrArgsItems<-paste0("\\item{ \\code{\\link{", attrArgs, "}}}")  
#     txt<-c(
#       paste("@name", name),
#       paste("@title", name), #todo!!! add something
#       "@description ",
#       description,
#       "@section Available Content Elements:",
#       "\\describe{",
#       elemArgsItems,
#       "}",
#       "@section Available Attributes:",
#       "\\describe{",
#       attrArgsItems,
#       "}",
#       "@keywords internal"
#     )
#     tmp<-paste("#' ", txt, sep="", collapse="\n")
#     tmp
#   }
#   
  
  unique(es.DT$element)->all.elements
  es.DT[variable=="content.model", list(content=list(I(.I))), by=element]->content.DT
  es.DT[variable=="attr",  list(attr=list(I(.I))), by=element]->attributes.DT
  eleL<-lapply( all.elements,
                function(elName){
                  content<-es.DT$value[ content.DT[element==elName]$content[[1]] ]
                  attr<-es.DT$value[attributes.DT[element==elName]$attr[[1]] ]
                  addElementEntry(name=elName, elemArgs=content, attrArgs=attr, 
                                  description="Description Needs to be Written!!!")
                }
    )
  rtv<-paste(eleL, "\nNULL\n", collapse="\n")
}

get.Attr.defs<-function(es.DT){
  es.DT[variable=="attr",]->attributes.DT
  attributes.DT[, list(elements=list(I(.I))), by=value]->tmp.DT
  all.attrs<-tmp.DT$value
  eleL<-lapply( all.attrs,
                function(attName){
                  eleIndices<-tmp.DT[value==attName,]$elements[[1]]
                  elemArgs<-attributes.DT$element[eleIndices] 
                  addAttributeEntry(name=attName, elemArgs=elemArgs, description="")
                }
  )
  rtv<-paste(eleL, "\nNULL\n", collapse="\n")
}


do.documentation<-function(es.DT, composerFiles="composerFiles"){
  #listing of all elements
#   eleAlphabeticalIndexDoc<-gen.all.Elem.Index(es.DT) 
#   cat(eleAlphabeticalIndexDoc, file=paste(composerFiles, "eleAlphabeticalIndexDoc.R", sep="/"))
  
  #listing of Element Categories
  ele.cat.indx<-generate.ele.cat.Index(es.DT)
  cat( ele.cat.indx, file=paste(composerFiles, " ele.cat.indx.page.R", sep="/") )
  
  #elecat doc
#   elemCatDoc<-get.Elem.categories(es.DT)
#   cat(elemCatDoc, file=paste(composerFiles, "elemCatDoc.R", sep="/") )
  
  #ele def doc
  ele.pages<-generate.element.pages(es.DT)
  cat(ele.pages, file=paste(composerFiles, "ele.pages.R", sep="/") )

  #attr doc
  attrDefDoc<-get.Attr.defs(es.DT)
  cat(attrDefDoc, file=paste(composerFiles, "attrDefDoc.R", sep="/") ) 
}

do.documentation(es.DT)
