
#todo 
# 1. Build svgEleDefs from svgFnQ
# 2. change in ComposerFiles svgDoc and display to use svgEleDefs
# 3. Add a copy fn to copy to Composer Project (or make a link)
# 4. 


eleDefBldr<-function(svgFnQ=svgFnQ,composerFiles="./ComposerFiles"){
  eleBld<-function(x){
    bd<-paste(deparse(body(svgFnQ[[name]])), collapse="\n")
    tmp<-paste('"',name,'"',"=function(...)\n",bd, sep="")   
  }
  fns<-sapply(names(svgFnQ), eleBld)
  eleDefs<-paste(fns,collapse=",\n")
  #next we write it to file
  cat("eleDefs<-list(\n",eleDefs,"\n)", 
      file=paste(composerFiles,"eleDefs.R", sep="/"))
}

#eleDefBldr()