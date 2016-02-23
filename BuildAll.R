
#  ------------------------------------------------------------------------


#  ------------------------------------------------------------------------


library(data.table)
#fread("./dataTables/elementSummary.tsv")->es.DT #triples: element, type, value 
#fread("dataTables/AVELTable.tsv")->AVEL.DT
#fread("dataTables/AVDTable.tsv")->AVD.DT
#fread("./dataTables/elementAttrCategorySummary.tsv")->eaCS.DT
#fread("dataTables/presentationAttr.tsv")->PA.DT
#fread("dataTables/comboParams.tsv")->COP.DT

source("./tableLoader.R")
requireTable( es.DT, AVEL.DT, AVD.DT, eaCS.DT, PA.DT, COP.DT)

buildAll<-function(targetDir="svgR"){ 
#code
  # provides the  svgFnQ function list creator 
  source("./svgcreatoR.R") 
  # provides the follow fn for writing element defs to file
  source("./eleDefBldr.R")
  eleDefBldr(svgFnQ, targetDir)
  
#documentation generation  
  source("docBldr.R") # entry point for documentmenation
  do.documentation(es.DT, targetDir) 

#lateX addon
  source("./TeXUnicodeBldr.R")
  TeXUnicodeBldr(targetDir)
}
buildAll()
