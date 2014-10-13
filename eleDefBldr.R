
#todo 
# 1. Build svgEleDefs from svgFnQ
# 2. change in ComposerFiles svgDoc and display to use svgEleDefs
# 3. Add a copy fn to copy to Composer Project (or make a link)
# 4. 

# eleBld<-function(nm){
#   bd<-paste(deparse(body(svgFnQ[[nm]])), collapse="\n")
#   tmp<-paste('"',nm,'"',"=function(...)\n",bd, sep="")
#   tmp
# }


eleDefBldr<-function(svgFnQ, composerFiles="./ComposerFiles"){

  tmp<-paste(deparse(svgFnQ),collapse="\n")
  cat("eleDefs<-",tmp, 
      file=paste(composerFiles,"eleDefs.R", sep="/"))
}

eleDefBldr(svgFnQ)


