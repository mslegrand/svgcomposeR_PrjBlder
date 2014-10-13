
library(data.table)

buildAll<-function(composerFiles="./ComposerFiles"){  
  source("docBldr.R")
  fread("./dataTables/elementSummary.csv")->es.DT #triples: element, type, value  
  do.documentation(es.DT)
  source("./svgcreatoR.R")
  source("./eleDefBldr.R")
  eleDefBldr(svgFnQ=svgFnQ, composerFiles=composerFiles) 
}
buildAll()
