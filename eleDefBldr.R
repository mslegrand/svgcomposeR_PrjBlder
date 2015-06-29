
#todo 
# 1. create treatAsRegAttr.builder 
# 2. use treatAsRegAttr to create fns
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


