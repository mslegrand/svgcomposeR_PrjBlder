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
fread("./dataTables/elementSummary.tsv")->es.DT #triples: element, type, value 
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


asDot<-function(aName){
  gsub('[-:]','.', aName)
}

#we need 
# 1. element name link: 
#   a. In genElemDoc
#       currently done in "elements.by.category.listing"
#       and appears to use the dot name 
#   b. reg attrs use dot name for elements via elementsRD
#   c. CO atttrs use dot name via elementsRD
#   d. pres attrs use dot name for elements via "elements.by.category.listing"

# 2. reg attr name link
#   are derived from the location in given in  AVEL.DT
#   
# 3. pres attr name link
#   derived by prepending presAttrs. and replacing - with dot
#   done in getPresAttrsLoc
# 4. comb attr name link
#    links are generated from AVEL.DT loc and co.loc2 and are dot
#  
# 4. attr value name link ??
# asRDLink<-function(aName){
#   title<-asDot
#   paste0("\\link[=", component.loc,"]{",component,"}")
#   dashName<-aName[indx]
#   if(length(dashName))
# }

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

source('genElemDocPages.R')

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
