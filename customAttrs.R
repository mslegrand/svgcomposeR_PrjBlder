# Custom attributes

# aev.

#search for x, y, z
#search for X, Y, Z

#search for x and y
#search for X and Y

#search for 1 and 2

#search for 1, 2, 3, 4, ...

#search for width and height
library(data.table)

elements<-unique(ave.DT$element)


el.contains.attrs<-function(el, sought.attrs){
  elAttr<-ave.DT[element==el]$attr
  prod(match(sought.attrs, elAttr, 0L))>0
}

# findAllElContainingAttrs<-function(...){
#   sought.attrs<-c(...)
#   unique(ave.DT$element)->elements
#   indx<-sapply(unique(ave.DT$element), el.contains.attrs, sought.attrs)
#   elements[indx]
# }

getDT.Containing.Attrs<-function(el, variable, sought.attrs){
  if( el.contains.attrs(el,sought.attrs) ){
    DT<-data.table(element=el, variable=variable, sought.attrs=sought.attrs)
  } else {
    DT<-data.table()
  }
  DT
}


# returns DT with attrs that match attrs ending in x
# that is look for attrs of el ending with x, then
# for each attrx ending in x, find all attr* that matches
# and return DT with 
# element=el, 
# variable=attrN, (attrN is the name attrxy.. where attry matches, ...)
# values=attr*
getDT.Matching.XEndAttrs<-function(el, x){
    xd<-paste0(x,'$')
    #cat(xd,"\n")
    elAttr<-ave.DT[element==el]$attr
    elAttrx<-elAttr[grep(xd,elAttr)]
    
    fn<-function(ax){
      pat<-paste0("^",substr(ax,1,nchar(ax)-nchar(x)),".$")
      #pat<-sub(xd,'.$', ax) 
      #cat(ax,ay,"\n")
      elMatch<-elAttr[grep(pat,elAttr)]
      if(length(elMatch)>1 ){ 
        len<-nchar(elMatch[1])
        suffix<-sort(substr(elMatch,len,len))
        prefix<-substr(elMatch[1],1,len-1)
        variable<-paste(c(prefix,suffix), collapse='')
        data.table(element=el, variable=variable , value=elMatch )
      } else
        data.table()    
    }
    
    tmp<-lapply(elAttrx, fn)
    rbindlist(tmp)   
}

# findBoth<-function(el, x, y){
#   xd<-paste0(x,'$')
#   #cat(xd,"\n")
#   elAttr<-ave.DT[element==el]$attr
#   elAttrx<-elAttr[grep(xd,elAttr)]
#   
#   fn<-function(ax){
#     ay<-sub(xd,y, ax) 
#     #cat(ax,ay,"\n")
#     if(ay %in% elAttr ){     
#       data.table(element=el, v1=ax, v2=ay , v12=paste0(ax,y) )
#     } else
#       data.table()    
#   }
#   
#   tmp<-lapply(elAttrx, fn)
#   rbindlist(tmp) 
# }
# 
# tmp<-sapply(elements, function(el){findBoth(el,"x","y")})
# rbindlist(tmp)->xy.DT
# 
# tmp<-sapply(elements, function(el){findBoth(el,"X","Y")})
# rbindlist(tmp)->XY.DT
# 
# tmp<-sapply(elements, function(el){findBoth(el,"1","2")})
# rbindlist(tmp)->t12.DT
# 


tmp<-c(
  lapply(elements, function(el){getDT.Matching.XEndAttrs(el,'x')}),
  lapply(elements, function(el){getDT.Matching.XEndAttrs(el,'X')}),
  lapply(elements, function(el){getDT.Matching.XEndAttrs(el,'1')}),
  lapply(elements, function(el){getDT.Containing.Attrs(el, 'wh', 
                                                       c('width','height'))}),
  lapply(elements, function(el){getDT.Containing.Attrs(el, 'in12', 
                                                       c('in', 'in2') )}),  
  lapply(elements, function(el){getDT.Containing.Attrs(el, 'xy1', 
                                                       c('x1', 'y1') )}),
  lapply(elements, function(el){getDT.Containing.Attrs(el, 'xy2', 
                                                       c('x2', 'y2') )}), 
  lapply(elements, function(el){getDT.Containing.Attrs(el, 'in1', 'in')}),
  lapply(elements, function(el){getDT.Containing.Attrs(el, '.', ':')}),
  lapply(elements, function(el){getDT.Containing.Attrs(el, '.', '-')})
)

rbindlist(tmp)->comboParams.DT

write.table(comboParams.DT,file="dataTables/comboParams.tsv",
            sep="\t",
            row.names=FALSE,
            quote=FALSE)

# write.table(es.DT,file="dataTableLink/elementSummary.tsv",
#             sep="\t",
#             row.names=FALSE,
#             quote=FALSE)


specParamsCXY.list<-lapply(elements, function(el){
    getDT.Containing.Attrs(el, 'cxy', 
                           c('width','height', 'x', 'y'))
  })

specParamsCXY.DT<-rbindlist(specParamsCXY.list)
write.table(specParamsCXY.DT,file="dataTables/specParamsCXY.tsv",
            sep="\t",
            row.names=FALSE,
            quote=FALSE)

# write.table(es.DT,file="dataTableLink/elementSummary.tsv",
#             sep="\t",
#             row.names=FALSE,
#             quote=FALSE)

# # preprocXtras
# # xy, cxy, rxy, xy1, xy2, wh
# attrSplitX<-function(attrs,  a1, a2, a12){
#   if(a12 %in% names(attrs)){
#     attrs[c(a1,a2)]<-attrs[[a12]]
#     attrs[[a12]]<-NULL
#   }
#   attrs
#}

# todo:
# presentation attribute
# required attrs???, required elements???
# allowable attrs, all attrs from AVE.DT and preprocAttr.DT, and attribAlias.DT and 
# special ... special should contain 
# cxy + width height =>xy
# cxy (text, tspan textPath)=> anchor, ...
# gradients colors, offsets, opacities => stops
# 

# todo: straight substitutions for dash, colon to dot (also maybe include in=>in1)
# todo: customize for c('text' , 'textPath' , 'tspan')
# (add -font) ) ))|((weight))|((variant))|((size))|((family 
#  anchor
# todo: customize for linearGradient",  "radialGradient"
#   colors, offsets, opacities 
# 
# todo: for stop, color=>stop-color, opacity=>stop-opacity???
# 
# todo: rgb???
# 
  

# 
# makePreProcSplitList<-function(){
#   a1<- c("horiz-origin-x","vert-origin-x", "dx", "x",  "xChannelSelector", "cx", "rx" ,  "x1" , "x2" , "fx")            
#   a2<-gsub("x","y",a1)
#   a12<-gsub("x","xy",a1)
#   b2<-c("g2",  "u2", "k2",  "x2",  "y2")
#   b1<-gsub("2","1",b2)
#   b12<-gsub("2","12",b2)  
#   as.list(data.frame(
#     cbind(
#       rbind("width","height","wh"),
#       rbind("in","in2","in12"),
#       rbind(a1,a2,a12),
#       rbind(b1,b2,b12)
#     ), stringsAsFactors=F , row.names=c("a1","a2","a12")
#   ))
#   tmp<-cbind(
#     rbind("width","height","wh"),
#     rbind("in","in2","in12"),
#     rbind(a1,a2,a12),
#     rbind(b1,b2,b12)
#   )
#   apply(tmp, 2, function(x)list(a1=x[1],a2=x[2], a12=x[3]))->tmplist
#   