
library(data.table)

buildAll<-function(composerFiles="composerFiles"){  
  source("docBldr.R")
  fread("./dataTables/elementSummary.csv")->es.DT #triples: element, type, value  
  do.documentation(es.DT, composerFiles=composerFiles) 
  source("./svgcreatoR.R")
  source("./eleDefBldr.R")
  eleDefBldr(svgFnQ, composerFiles=composerFiles) 
}
buildAll()
