
library(data.table)
fread("./dataTables/elementSummary.tsv")->es.DT #triples: element, type, value 
fread("dataTables/AVELTable.tsv")->AVEL.DT
fread("dataTables/AVDTable.tsv")->AVD.DT

buildAll<-function(composerFiles="composerFiles"){  
  #fread("./dataTables/AVDTable.tsv")->AVD.DT
  source("docBldr.R")
  do.documentation(es.DT, composerFiles=composerFiles) 
  source("./svgcreatoR.R")
  source("./eleDefBldr.R")
  eleDefBldr(svgFnQ, composerFiles=composerFiles)
  source("./TeXUnicodeBldr.R")
  TeXUnicodeBldr(composerFiles=composerFiles)
}
buildAll()
