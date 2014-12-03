
library(data.table)

buildAll<-function(composerFiles="composerFiles"){  
  source("docBldr.R")
  fread("./dataTables/elementSummary.tsv")->es.DT #triples: element, type, value 
  fread("dataTables/AVELTable.tsv")->AVEL.DT
  #fread("./dataTables/AVDTable.tsv")->AVD.DT
  fread("dataTables/AVDTable.tsv")->AVD.DT
  do.documentation(es.DT, composerFiles=composerFiles) 
  source("./svgcreatoR.R")
  source("./eleDefBldr.R")
  eleDefBldr(svgFnQ, composerFiles=composerFiles)
  source("./TeXUnicodeBldr.R")
  TeXUnicodeBldr(composerFiles=composerFiles)
}
buildAll()
