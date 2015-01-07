
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


eleDefBldr<-function(svgFnQ, targetDir="svgR"){

  tmp<-paste(deparse(svgFnQ),collapse="\n")
  #tmp<-gsub('}, ', "}, \n",tmp)
  tmp3<-gsub('}, ', "}, \n",tmp)
  cat("eleDefs<-\n",tmp3, 
      file=paste(targetDir,"eleDefs.R", sep="/"))
}

#eleDefBldr(svgFnQ)


