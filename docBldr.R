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
library(data.table)

source("./tableLoader.R")

source("./commonDoc.R")

requireTable( es.DT, eaCS.DT, PA.DT, COP.DT)

#buildDocumentation

#------------------------ATTENTION!!!!-----------------------------------------
# tmp kludge to remove the presentation attrs
#------------------------BEGIN KLUDGE!!!!-----------------------------------------
eaCS.DT[name!="presentation attributes"]->eaCS.DT
#rbind(eaCS.DT, data.table(name="presentation attributes", value="alignment-baseline"))
#------------------------END KLUDGE!!!!-----------------------------------------

source('genElemDocPages.R')

# requires es.DT, AVEL.DT, AVD.DT,
do.documentation<-function(es.DT, composerFiles="svgR"){ 
  source("genAttrDocPages.R")
  
  #listing of Elements by Categories
  ele.cat.indx<-generate.ele.cat.Index()
  cat( ele.cat.indx, file=paste(composerFiles, "doc_EleCatIndxPage.R", sep="/") )
  
  #individual element documentation
  ele.pages<-generate.element.pages()
  cat(ele.pages, file=paste(composerFiles, "doc_ElePages.R", sep="/") )

  #regular attribute documentation
  regAttrDocPages<-generate.Reg.Attr.Pages()
  cat(regAttrDocPages, file=paste(composerFiles, "doc_RegAttrPages.R", sep="/") )

  #presentation Attributes
  presAttrDocPages<-generate.Pres.Attr.Pages()
  cat(presAttrDocPages, file=paste(composerFiles, "doc_PresAttrPages.R", sep="/") )

  #comboAttrDocPages
  combAttrDocPages<-generate.CO.Attr.Pages()
  cat(combAttrDocPages, file=paste(composerFiles, "doc_CombAttrPages.R", sep="/") )
  
#attr doc
  #attrDefDoc<-get.Attr.defs(es.DT)
  #cat(attrDefDoc, file=paste(composerFiles, "attrDefDoc.R", sep="/") ) 
}

do.documentation(es.DT)
