
source("R/TeXUniCode.R")

tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))
  
  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))
  
  cols <- lapply(df, format, ...)
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n#'   ")))
  
  paste("#'  \\tabular{", paste(col_align, collapse = ""), "}{\n#'  ",
        contents, "\n#'  }\n", sep = "")
}


#we process tmp
tmp<-names(TeXUniCode)
tmp[which(nchar(tmp)>1)]->tmp #remove single characters
tmp[-grep("\\{",tmp)]->tmp #remove {}
tmp[-grep("\\}",tmp)]->tmp
tmp

#cat(tabular(mtcars[1:5, 1:5]))
n<-4
#paste("'\\",tmp,"'", sep="")->tmps
#tmps<-gsub("\\\\","\\\\\\\\",tmp)
tmps<-gsub("\\\\","",tmp)
tmps<-gsub("%","\\\\%",tmps)
# tmpss1<-gsub("\\{", "\\\\{",tmps)
# tmpss2<-gsub("\\}", "\\\\}",tmpss1)
tmps<-sort(tmps)
tmps<-paste("\\\\\\\\",tmps,sep="")
tmps<-paste('"',tmps,'"',sep="")
#tmpm<-as.data.frame(matrix(c(tmpss3,rep(" ",length(tmpss3)%%n)),,n))
tmpm<-as.data.frame(t(matrix(c(tmps,rep(" ",length(tmpss3)%%n)),n,)))

#ltmpm<-as.data.frame(matrix(tmp,,3))
cat(tabular(tmpm), file="tmp.txt")

