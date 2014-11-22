
#utils:::.addFunctionInfo(svg=c("cat","dog"))

library(data.table)
library(XML)

#Done
# X 1. make a dot alias for all fns with dash, ie  make font.face for "font-face" (already done for attributes)
#  X 3. if an attr is unnamed but of class list,  promote it's members as children.
#   a. collect all unamed args which are themselfs a list, unL
#   b. remove unl from args and promote each member to attr (shall this be recursive??)
#   c. .children will be unamed args that are either character or numeric or xmlNode

#todo:
# ?  change project name from svgCreator to svgComposer
# ?2. make a default for c(x,y) to be space seperated (check to see if this is already the case)
# ?4. What happens to a non-list (vector) that should be ws seperated (or sc or cl ...)?
# 5. Add function completions HOW???
# 6. Add documentation HOW???
# 7. Add sample programs 
# 8. Add unit testing (regression testing?) and other methods
# 9.Animation:
#    doc['initialize']
#    doc["set"]( color(id)="red", ...)
#    doc["animate"]( XY(id)=c(1,2), xy(id)=(1,2), ...)
#    or 
#    doc[[id]], replace by doc$build(rootNode=id,...) 
#           or build(doc, rootNode=id,  ...)
#           or doc$root(...)
#    doc["set"], replace by doc$set(...) or  set(doc, ...)
#    doc["animate], replace by doc$animate(...) or animate(doc, ...)

#   Do we need 
#       1. ids(doc) ? (return all ids)
#       2. def(doc) ? (return ids of defs)
#       3. svgnode(id) ?
#       4. 
#       

# for filter should we always append % to the x,y width, heigth values???
# k for k1,k2,k3,k4 in feComposite ?


#1. how to add function completion:
# i) utils:::.addFunctionInfo(fn=c("cat","dog")) #note 3 colons
# ii)alternatively: 
#    pkgEnv = getNamespace("MyPackage")
#    attach(pkgEnv)
#2. How to add function documentation???


# done, not tested!
# 1. x,y => xy for all of
# "horiz-origin-x"   "vert-origin-x"    "dx"               "x"                
# "xChannelSelector"
#  "cx"               "rx"               "x1"               "x2"               "fx"              
# generate list by:
# unique(ave.DT[attr %like% "y",]$attr)->tmp2
# sub("y","x",tmp2)->tmp3
# unique(ave.DT$attr[sapply(ave.DT$attr, function(attr)attr %in% tmp3)] )

# 1,2 =>12 for all of "g2"  "u2"  "in2" "k2"  "x2"  "y2" 

# inn for in, in2 (or just inn for in???) or should we use .in???
# or should we use in1 in2 and in12 ??
#
# u12 for u1, u2 (or should we use u)
# g12 for g1, g2 
# fxy for fx, fy in radial gradient
# xlink:href as xlink..href???
# what about the following
# "xlink:href"    "xml:base"      "xml:lang"      "xml:space"     "xlink:actuate" "xlink:arcrole" "xlink:role"    "xlink:show"    "xlink:title"   "xlink:type" 
# generate list by:
# unique(ave.DT[attr %like% ":",]$attr)
#

#the following line is required because of a bug in devtools (maybe they will fix it some day)
.datatable.aware=TRUE

fread("./dataTables/AVETable.csv")->ave.DT
fread("dataTables/comboParams.tsv")->comboParams.DT


# Builds the svgFnQ stuff
build.svgFnQ<-function(){
  ele.tags<-unique(ave.DT$element)
  
#   splitAtt<-function(etag, x){
#     ifelse(
#       nrow(ave.DT[element==etag & (attr==x['a1'] | attr==x['a2']) ,])==2,
#       paste("attrs<-attrSplitX(attrs, '" ,x['a1'], "','" ,x['a2'], "','" ,x['a12'], "')", sep=""),
#       ""
#     )
#   }
  
  centerable<-function(ele.tag, ave.DT){
    ifelse(
      nrow(ave.DT[  element==ele.tag & 
                      (attr=='x' | attr=='y' | attr=='width' | attr=='height') ,]
      )==4,
      "attrs<-mapCenteredXY(attrs)",
      ""
    )  
  }
  
  
  makePreProcSplitList<-function(){
    a1<- c("horiz-origin-x","vert-origin-x", "dx", "x",  "xChannelSelector", "cx", "rx" ,  "x1" , "x2" , "fx")            
    a2<-gsub("x","y",a1)
    a12<-gsub("x","xy",a1)
    b2<-c("g2",  "u2", "k2",  "x2",  "y2")
    b1<-gsub("2","1",b2)
    b12<-gsub("2","12",b2)  
    as.list(data.frame(
      cbind(
        rbind("width","height","wh"),
        rbind("in","in2","in12"),
        rbind(a1,a2,a12),
        rbind(b1,b2,b12)
      ), stringsAsFactors=F , row.names=c("a1","a2","a12")
    ))
    tmp<-cbind(
      rbind("width","height","wh"),
      rbind("in","in2","in12"),
      rbind(a1,a2,a12),
      rbind(b1,b2,b12)
    )
    apply(tmp, 2, function(x)list(a1=x[1],a2=x[2], a12=x[3]))->tmplist
  }
  
  preprocSplitList<-makePreProcSplitList()
  
  # "ignore cmm-list path-data-list wsp-list scln-list cmm-scln-list number-optional-number cln-scln-list cmm-wsp-list transform-list"
  createEleFnQ<-function(ele.tag, ave.DT){
    ave.DT[element==ele.tag & treatValueAs!="ignore",]->ele.dt
    ele.treatments<-unique(ele.dt$treatValueAs)
    ele.dt[, paste(attr, collapse=" "), by=treatValueAs]->treat_attrs.dt
    #This is the extras 
    body0<-c(
      quote( args <- list(...) ),
      quote( args <- promoteUnamedLists(args) ),
      quote( attrs <- named(args) )
    )
    
    qcomboParamsFn<-function(etag){
      tmp<-comboParams.DT[element==etag]
      if(nrow(tmp)>0){
        cp.list<-split(tmp$value, tmp$variable)
        # for each element of tmp.list, add the appropriate quote
        substitute(attrs<-comboParamHandler(attrs, cp ), list(cp=cp.list))
      } else {
        quote(NULL)
      }
    }
    
    #this makes the attrSplitX call
#     qPreproXtrasFn<-function(x, etag){
#       if(nrow(ave.DT[element==etag & (attr==x$a1 | attr==x$a2) ,])==2){
#         substitute(attrs<-attrSplitX(attrs, a1, a2, a12), x)
#       } else {
#         quote(NULL)
#       }
#     }
#     
    
    ppXtraCL<-list( qcomboParamsFn(ele.tag) )
    
#   add all attrSplitX calls  
#   lapply(preprocSplitList, qPreproXtrasFn, etag=ele.tag )->ppXtraCL #a list of calls
#     
    if(nrow(ave.DT[element==ele.tag & (attr=='x' | attr=='y' | attr=='width' | attr=='height') ,])==4 ){
      ppXtraCL<-c(ppXtraCL, quote(attrs<-mapCenteredXY(attrs) ) ) # append a call
    }
    
    ppXtraCL[sapply(ppXtraCL, is.null)] <- NULL #remove any nulls
    body1<-ppXtraCL
    
    # add code to treat special lists, ie. comma list, space list, semicolon list ...
    split(treat_attrs.dt, rownames(treat_attrs.dt))->tmp # (convert rows of treat_attrs.dt table to list)  
    preprocAttrValueFn<-function(tvaAttr){
      c(
        substitute( indx<-sapply(names(attrs),function(x)grepl(paste('(^| )',x,'($| )',sep=''), V1 )),tvaAttr),      
        substitute( if(length(indx)>0){ attrs[indx]<-lapply(attrs[indx], function(x){ svgPreproc[[treatValueAs]](x) })}, tvaAttr)
      )
    } 
    body2<-lapply(tmp, function(tvaAttr){preprocAttrValueFn(tvaAttr)}) 
    unlist(body2, use.names=F)->body2
    as.list(body2)->body2

    #add code to add to node children
    body3<-substitute(node<-newXMLNode(ele.tag, attrs=attrs, .children=allGoodChildern(args)), list(ele.tag=ele.tag))
    
    #special cases for text (may replace this later)
    if(ele.tag %in% c('text' , 'textPath' , 'tspan')){
      body3<-c(
        quote(if(!is.null(names(attrs))){
          attr.names<-names(attrs)
          attr.names<-gsub("^(((style))|((weight))|((variant))|((size))|((family)))$", "font-\\1",attr.names, fixed=F)
          attr.names<-gsub("^anchor$","text-anchor",attr.names)
          names(attrs)<-attr.names
          if(!is.null(attrs[["cxy"]])){
            attrs[["text-anchor"]]<-'middle'
            attrs[["dominant-baseline"]]="central"
            attrs[["xy"]]=attrs[["cxy"]]
            attrs[["cxy"]]=NULL
          }
          attrs<-mapArg(attrs,"xy", c("x","y"))
          text<-NULL
          if("text" %in% attr.names){ ### use value instead of text???
            text<-attrs["text"]
            attrs["text"]<-NULL
          }
        }),
        body3
      )    
    }
    #special code for gradients
    if(ele.tag %in% c("linearGradient",  "radialGradient")){
      body3<-c(
        quote(
          if("colors" %in% names(attrs)){
            colors<-attrs[["colors"]]
            attrs[["colors"]]<-NULL
            if("offsets" %in% names(attrs)){
              offsets<-attrs[["offsets"]]
              attrs[["offsets"]]<-NULL
            } else {
              offsets<-seq(0,100,length.out=length(colors))
            }
            for(i in 1:length(colors)){
              attrs.si<-list(offset=sprintf("%d%%", offsets[i]), "stop-color"= colors[i])
              stopi<-newXMLNode("stop", attrs=attrs.si)
              args<-c(args,stopi)
            }
          }),
        body3
      )    
    } 
    fn<-function(...){}
    body(fn)<-as.call(c(as.name("{"), body0, body1, body2, body3))
    fn  
    
  }
  
  svgFnQ<-lapply(ele.tags, createEleFnQ, ave.DT=ave.DT )
  names(svgFnQ)<-ele.tags
  
  #here we handle names with -
  indx<-grep("-", names(svgFnQ))
  tmpFn<-svgFnQ[indx]
  names(tmpFn)<-gsub("-",".",names(tmpFn))
    
  svgFnQ<-c(svgFnQ, tmpFn,
            list(
              svgDoc=function(width=1150, height=860,  ... ){
                args<-unlist(c(list( width=width, height=height), list(...)))
                #              namespaceDefinitions<- list(
                #                "http://www.w3.org/2000/svg",
                #                xlink="http://www.w3.org/1999/xlink"
                #              )
                namespaceDefinitions<-list(
                  xmlns="http://www.w3.org/2000/svg",
                  xmlns="http://www.w3.org/1999/xlink")
                #'xmlns:xlink'="http://www.w3.org/1999/xlink")
                root<-newXMLNode("svg", attrs=named(args), namespaceDefinitions = namespaceDefinitions, .children=unnamed(args))
                ensureNamespace(root, c(xlink="http://www.w3.org/1999/xlink"))
                root
              },
              svgMarkup.new=function(width=1150, height=860, 
                                      namespaceDefinitons=NULL,  ... ){
                args<-c(list( width=width, height=height), list(...))
                if( is.null(args[["namespaceDefinitons"]]) ){
                  namespaceDefinitions<- c(
                    "http://www.w3.org/2000/svg",
                    xlink="http://www.w3.org/1999/xlink",
                    ev="http://www.w3.org/2001/xml-events"
                  )
                } else {
                  namespaceDefinitions<-args[["namespaceDefiniton"]]
                  args[["namespaceDefiniton"]]<-NULL
                } 
                args[["id"]]<-"rootNode"
                args <- promoteUnamedLists(args)
                attrs <- named(args)
                attrs <- attrSplitX(attrs, "width", "height", "wh")
                attrs <- attrSplitX(attrs, "x", "y", "xy")
                attrs <- mapCenteredXY(attrs)
                indx <- sapply(names(attrs), function(x) grepl(paste("(^| )", 
                                                                     x, "($| )", sep = ""), "requiredExtensions requiredFeatures class preserveAspectRatio"))
                attrs[indx] <- lapply(attrs[indx], function(x) {
                  svgPreproc[["wsp-list"]](x)
                })
                indx <- sapply(names(attrs), function(x) grepl(paste("(^| )", 
                                                                     x, "($| )", sep = ""), "systemLanguage viewBox"))
                attrs[indx] <- lapply(attrs[indx], function(x) {
                  svgPreproc[["cmm-list"]](x)
                })
                indx <- sapply(names(attrs), function(x) grepl(paste("(^| )", 
                                                                     x, "($| )", sep = ""), "style"))
                attrs[indx] <- lapply(attrs[indx], function(x) {
                  svgPreproc[["cln-scln-list"]](x)
                })
                rootNode<-newXMLNode("svg", attrs=attrs, 
                                     namespaceDefinitions = namespaceDefinitions, 
                                     .children=unnamed(args)) 
                
                #todo: add options? (such as duration)
                doc<-structure(list(rootNode=rootNode, time=0), class="svgDoc", 
                               wh=c(width,height), delta=1)
                doc
              },
              getNode=function(rootNode,id){
                if(id!='root'){
                  kidV <- getNodeSet(rootNode, paste("//*[@id=\"", id, "\"]", sep=""))
                } else {
                  kidV <- list(rootNode)
                }
                if (length(kidV)==0){
                  stop("Cannot find node with id=",id)
                }
                kidV
              }
#               
#               getNode=function(rootNode,id){
#                 kidV<-getNodeSet(rootNode, paste( '//*[@id="',id,'"]' ) )
#               }         
  )
  )
  svgFnQ
}

svgFnQ<-build.svgFnQ()




