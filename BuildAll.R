
library(data.table)
fread("./dataTables/elementSummary.tsv")->es.DT #triples: element, type, value 
fread("dataTables/AVELTable.tsv")->AVEL.DT
fread("dataTables/AVDTable.tsv")->AVD.DT
fread("./dataTables/elementAttrCategorySummary.tsv")->eaCS.DT
fread("dataTables/presentationAttr.tsv")->PA.DT
fread("dataTables/comboParams.tsv")->COP.DT

buildAll<-function(composerFiles="composerFiles"){  
  source("./svgcreatoR.R") #this MUST come first, because later COP is reorder!
  source("docBldr.R")
  do.documentation(es.DT, composerFiles=composerFiles) 
  source("./eleDefBldr.R")
  eleDefBldr(svgFnQ, composerFiles=composerFiles)
  source("./TeXUnicodeBldr.R")
  TeXUnicodeBldr(composerFiles=composerFiles)
}
buildAll()
