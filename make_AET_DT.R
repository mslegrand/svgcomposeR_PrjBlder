library(data.table)

if(!exists("AVEL.DT")){
  fread("./dataTables/AVELTable.tsv")->AVEL.DT  
}

preproc.treat.val.as<-function(v){
  tmp<-c(
    "cmm-list\\s+\\{4\\}"="cmm-list",
    "default"="ignore",
    "filterprimitiveinattribute"="ignore",
    "integer"="ignore",
    "pointsbnf"="cmm-wsp-list",
    "transformlist"="transform-list",
    "lengths"="wsp-list",
    "numbers"="wsp-list",
    "coordinates"="wsp-list",
    "special-string"="wsp-list",
    "funciri"="ignore")
  
  for( n in names(tmp)){
    v<-gsub(n, tmp[n], v)
  }
  v  
}

#preprocess AVEL.DT
AVEL.DT[,treatValueAs:=preproc.treat.val.as(treatValueAs)]
AVEL.DT[,list(attr,element,anim,treatValueAs)]->AET.DT
write.table(AET.DT,file="./dataTables/AETTable.tsv",
            sep="\t",
            row.names=FALSE,
            quote=FALSE)

# write.table(tmp.list,file="./dataTables/AnimateParamLookupTable.tsv",
#             sep="\t",
#             row.names=FALSE,
#             quote=FALSE)
