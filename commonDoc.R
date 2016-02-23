# common doc building operations


capitalizeIt<-function(name){
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", name, perl=TRUE)
}


asDot<-function(aName){
  gsub('[-:]','.', aName)
}


nameWithLink<-function(aName, aLink=NULL){
  if(is.null(aLink)){
    aLink<-aName
  }
  aName<-asDot(aName)
  #paste0("\\link[=", aLink,"]{",aName,"}")
  paste0("\\code{\\link[=", aLink,"]{",aName,"}}")
}

rd.code<-function(x){
  paste0("\\code{",x,"}")
}

rd.item<-function(x,y=""){
  paste0("\\item{", x, "}{", y,"}")
}

rd.comma<-function(x){
  paste0(x, collapse=", ")
}


rd.emph<-function(x){
  paste0("\\emph{",x,"}")
}
rd.describe<-function(x){
  c("\\describe{", x, "}")
}
rd.itemize<-function(x){
  c("\\itemize{", x, "}")
}
rd.name<-function(x){
  paste("@name",x)
}
rd.title<-function(x){
  paste("@title",x)
}
rd.aliases<-function(x){
  paste("@aliases",paste0(x,collapse=" "))
}
rd.section<-function(x){
  paste0("@section ",x,":")
}
rd.keywords<-function(x){
  paste0("@keywords ",x)
}
rd.description<-function(x){
  c("@description ",x)
}
rd.param<-function(param, param.def){
  paste("@param ", param, param.def, sep="   ")
}

rd.usage<-function(x){
  c("@usage", paste(x, collpase="; "))
}

rd.details<-function(x){
  c("@details",x)
}

rd.close<-function(xs){
  tmp<-paste0("#' ", xs ) 
  c(tmp,"NULL","\n")
}

