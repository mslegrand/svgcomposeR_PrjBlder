
library(data.table)
fread("./dataTables/elementSummary.tsv")->es.DT #triples: element, type, value 
fread("dataTables/AVELTable.tsv")->AVEL.DT
fread("dataTables/AVDTable.tsv")->AVD.DT
fread("./dataTables/elementAttrCategorySummary.tsv")->eaCS.DT
fread("dataTables/presentationAttr.tsv")->PA.DT
fread("dataTables/comboParams.tsv")->COP.DT

buildAll<-function(targetDir="svgR"){  
  source("./svgcreatoR.R") #this MUST come first, because later COP is reorder!
  source("./eleDefBldr.R")
  eleDefBldr(svgFnQ, targetDir)
  
  source("docBldr.R")
  do.documentation(es.DT, targetDir) 
  
  source("./TeXUnicodeBldr.R")
  TeXUnicodeBldr(targetDir)
}
buildAll()
