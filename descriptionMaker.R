library(data.table)
fread("dataTables/ECD.tsv")->ECD.DT

split(ECD.DT,ECD.DT$category)->tmp


tl<-sapply(names(tmp),
       function(n){
           paste0(tmp[[n]]$element, 
                 " = c(\n    '',\n    \"",
                 tmp[[n]]$description,'"\n)', collapse="\n"
          )       
       }
)

tv<-unlist(tl)
tc<-paste(tv, collapse=",\n")
#cat(tv, file="description.txt")
# sort(unique(ECD.DT$category))->cats
# 
#   ECD.DT[, list(.SD),  by=category]->tmp
