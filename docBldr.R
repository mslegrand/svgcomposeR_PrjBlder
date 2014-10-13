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

addEleCategoryEntry<-function(name, elemArgs, description="", visible=TRUE ){
    txt<-c(
        paste("@name", name),
        paste("@title", name), #!!!todo add something
        paste("@description ", description),
        paste("\\itemize{"),
        paste(" \\item{}{ \\code{\\link{", elemArgs, "}}}", sep=""),
        "}"
    )
    tmp<-paste("#' ", txt, sep="", collapse="\n")
}

addElementEntry<-function(name, elemArgs, attrArgs, description=""){
  elemArgsF<-paste("\\code{\\link{", elemArgs, "}}", sep="", collapse=", ")
  attrArgsF<-paste("\\code{\\link{", attrArgs, "}}", sep="", collapse=", ")  
  txt<-c(
    paste("@name", name),
    paste("@title", name), #todo!!! add something
    paste("@description ", description),
    paste("\\itemize{"),
    paste(" \\item{Available Content Elements}{ ",   elemArgsF, "}", sep=""),
    paste(" \\item{Available Attributes}{ ", attrArgsF, "}", sep=""),
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


#returns txt for element cat list ing doc
get.Elem.categories<-function(es.DT){
  es.DT[type=="category", list(list(I(.I))), by=value]->tmp
  catsL<-sapply(1:nrow(tmp), 
    function(i){ addEleCategoryEntry(name=tmp$value[i], elemArgs= es.DT$element[ tmp$V1[[i]] ] )}
  )
  rtv<-paste(catsL, "\nNULL\n", collapse="\n")
}

get.Element.defs<-function(es.DT){
  unique(es.DT$element)->all.elements
  es.DT[type=="content.model", list(content=list(I(.I))), by=element]->content.DT
  es.DT[type=="attr",  list(attr=list(I(.I))), by=element]->attributes.DT
  eleL<-lapply( all.elements,
                function(elName){
                  content<-es.DT$value[ content.DT[element==elName]$content[[1]] ]
                  attr<-es.DT$value[attributes.DT[element==elName]$attr[[1]] ]
                  addElementEntry(name=elName, elemArgs=content, attrArgs=attr, description="")
                }
    )
  rtv<-paste(eleL, "\nNULL\n", collapse="\n")
}

get.Attr.defs<-function(es.DT){
  es.DT[type=="attr",]->attributes.DT
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


do.documentation<-function(es.DT, composerFiles="./ComposerFiles"){
  #elecat doc
   elemCatDoc<-get.Elem.categories(es.DT)
   cat(elemCatDoc, file=paste(composerFiles, "elemCatDoc.R", sep="/") )
  #ele def doc
  elemDefDoc<-get.Element.defs(es.DT)
  cat(elemCatDoc, file=paste(composerFiles, "elemDefDoc.R", sep="/") )
  #attr doc
  attrDefDoc<-get.Attr.defs(es.DT)
  cat(elemCatDoc, file=paste(composerFiles, "attrDefDoc.R", sep="/") ) 
}
