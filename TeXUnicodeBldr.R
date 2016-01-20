# msymbol<-parse(text=shQuote("\\u222B"))[[1]]
# #web-safe fonts:http: //www.w3schools.com/cssref/css_websafe_fonts.asp
# #font-family  The font to use, for instance 'Arial' or 'Verdana'.
# doc[["root"]](text(id="my.text", font.size=36, font.family="Arial", x=50, y=50, msymbol))
# as.character(doc)
#
TeXUnicodeBldr<-function(composerFiles="composerFiles"){
  library(data.table)
  latUni.DT<-fread("./dataTables/Unicode/unicodeLatex.csv")
  latUni.DT[LaTeX!=""]->latUni.DT
  latUni.DT<-latUni.DT[ !duplicated(LaTeX), ]
  latUni.DT<-latUni.DT[ substr(no.,1,1)=="0"]
  latUni.DT<-latUni.DT[ , uni:=sub("0","\\\\u", no.)]
  
  
  
  latUni.V<-latUni.DT$uni
  
  names(latUni.V)<-latUni.DT$LaTeX
  
  # TeX2Uni<-sapply(latUni.V, function(x){parse(text=shQuote(x))[[1]]})
  
  #deparse(TeX2Uni)->tmp
  deparse(latUni.V)->tmp
  cat("TeXUniCode<-",tmp, "\n\nTeXUniCode<-sapply(TeXUniCode, function(x){parse(text=shQuote(x))[[1]]})\n",
      file=paste(composerFiles,"TeXUniCode.R", sep="/"))  
}
