
library(data.table)

buildAll<-function(composerFiles="composerFiles"){  
  source("docBldr.R")
  fread("./dataTables/elementSummary.tsv")->es.DT #triples: element, type, value  
  do.documentation(es.DT, composerFiles=composerFiles) 
  source("./svgcreatoR.R")
  source("./eleDefBldr.R")
  eleDefBldr(svgFnQ, composerFiles=composerFiles)
  source("./TeXUnicodeBldr.R")
  TeXUnicodeBldr(composerFiles=composerFiles)
}
buildAll()
