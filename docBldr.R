# good read: http://www.quantumvibe.com/strip?page=1

#ghost docs
#todo!!! 
# add cxy, ... and other custom attributes (i.e for gradient)
# add alias for . and - and :  elements
# add alias for . and - (and :? ) attributes
# add description
# add meaningful titles
# rename element categories (ends and refererences ending with :)
# add categories for attributes (such as presentation, ...)
# implement the name completions in .onload
# add a main category page
# add example docs
# check if we still need an alias for add attribute entry
# compare avel with attr


# todo!!! 
# save docs to link to svgComposer
# save source to link to svgComposer
# use svgComposer in svgShiny
# write examples!!!

#testing 123

#buildDocumentation
library(data.table)
#fread("./dataTables/elementSummary.csv")->es.DT #triples: element, type, value
fread("./dataTables/elementAttrCategorySummary.tsv")->eaCS.DT

eCS.DT<-eaCS.DT[grep("elements$", eaCS.DT$name)]
aCS.DT<-eaCS.DT[grep("attributes$", eaCS.DT$name)]



capitalizeIt<-function(name){
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", name, perl=TRUE)
}

addEleCategoryEntry<-function(name, elemArgs, description="", visible=TRUE ){
  txt<-c(
        paste0("@name ", gsub(' ','',name),"s"), #blue
        paste0("@title ", name,"s"), #!!!todo add something
        paste("@description ", description),
        paste("\\itemize{"),
        paste(" \\item \\code{\\link{", elemArgs, "}}", sep=""),
        "}",
        "@keywords internal"
    )
    tmp<-paste("#' ", txt, sep="", collapse="\n")
}




#generates an alphebetical Index of All Elements (no longer using)
gen.all.Elem.Index<-function(es.DT){
  unique(es.DT$element)->ele
  sort(gsub("-",".", ele))->ele
  ele<-paste(" \\item \\code{\\link[svgcomposeR]{", ele, "}}", sep="")
  txt<-c(
    paste("\\itemize{"),
    ele,
    "}",
    paste("@name",  "All Elements Index- Alhabetically"),
    paste("@title", "An Alphabetical Index of All Elements")
  )
  tmp<-paste("#' ", txt, sep="", collapse="\n")  
  tmp<-paste(tmp,"\nNULL\n") 
}

# Generates an Listing of Element Categories (Index of Ele Cats)
# Generates an Listing of Element Categories (Index of Ele Cats)
generate.ele.cat.Index<-function(){
  
  oneCatListing<-function(category){
    #category name
    es.DT[variable=="category" & value==category]$element->ele    
    sort(gsub("-",".", ele))->ele # convert - to . in element names
    res<-c(
      paste0("@section ",category,"s:"),
      "\\describe{",
      paste0("\\item{ \\code{\\link[svgcomposeR]{",ele,"}}}{}"),
      #"}",
      "}"
    )
    paste(res, collapse="\n#' ")    
  }
  cats<-unique(es.DT[variable=="category"]$value)
  cats<-sort(cats)
  # Element Group Name
  sapply(cats, oneCatListing)->cat.index
  c(
    cat.index,
    "@name SVG Element Creators",
    "@title Elements Indexed by Category"  
  )->cat.index 
  paste(cat.index, collapse="\n#' ")->cat.index 
  paste0("#' ",cat.index, "\nNULL\n")
}


#returns doc listing elements for each category
get.Elem.categories<-function(es.DT){
  es.DT[variable=="category", list(list(I(.I))), by=value]->tmp
  tmp<-tmp[order(value)]
  catsL<-sapply(1:nrow(tmp), 
    function(i){ addEleCategoryEntry(name=tmp$value[i], elemArgs= es.DT$element[ tmp$V1[[i]] ] )}
  )
  rtv<-paste(catsL, "\nNULL\n", collapse="\n")
}



#generates element documentation  for each element found in es.DT
# resulting for each element
# element name, content.model, attributes
generate.element.pages<-function(){

  #begin content element handleing
  #return all members of a given cat
  expand.Cat<-function(cat.name){
    eaCS.DT[name==cat.name]$value->members
  }
  
  expand.arg.names<-function(arg.names){
    fn<-function(x){
      if(grepl(":$",x)){
        xx<-gsub(":$","",x)
        x<-eaCS.DT[name==xx]$value
      } else {
        x
      }    
    }    
    unlist(lapply(arg.names, fn))   
  }
  
  extract.CatMember.List<-function(members, other="other"){
    #expand??
    #other<-"Other"
    if(length(members)==0){
      tmp.list<-list()
    } else {
      rowNum<-match(members, eaCS.DT$value, nomatch=0L)
      missing<-members[rowNum==0]
      tmp.DT<-eaCS.DT[value %in% members]
      cats<-sort(unique(tmp.DT$name))
      if(length(missing)>0){
        cats<-c(cats,other)
        tmp.DT<-rbind(tmp.DT, data.table(name=other, value=missing))
      } 
      tmp.list<-structure(lapply(cats, function(kit)tmp.DT[name==kit]$value) ,
                     names=cats)
      #tmp.list<-sort(gsub("[-:]",".", tmp.list)) # convert - to . in values
    } 
    tmp.list
  }
  
  
  
  #returns all the elements in the given category
  get.All.EleInGrp<-function(category){
    category<-sub("s$","",category)
    es.DT[variable=="category" & value==category]$element->ele 
    if(length(ele)==0){
      ele<-"Empty"
    }
    ele
  }
  
#   #returns a named vector giving the locations
#   getAttrLocation<-function(attrArgs, elName){
#    # AVEL.DT[attr==attrArgs & element==elName]$loc 
#     aCS.DT[name=="presentation attributes"]$value->presAttrs
#     gsub("[-:]","."presAttrs)->presAttrsV
#     AL.DT<-
#       rbind( AVEL.DT[element==elName, list(attr, loc)],
#              data.table[attr=presAttrsV, loc=attrpresV]
#     #append to this the location of any presentation elements
#     structure(AL.DT$loc, names=AL.DT$attr)
#   }
  
  makeAttrLinkItems<-function(attrArgs, elName){
    # AVEL.DT[attr==attrArgs & element==elName]$loc 
    if(length(attrArgs)>0){
      aCS.DT[name=="presentation attributes"]$value->presAttrs
      gsub("[-:]",".",presAttrs)->presAttrsV
      AL.DT<-
        rbind( AVEL.DT[element==elName, list(attr,  loc)],
               data.table(attr=presAttrs, loc=presAttrsV)
        )    
      dest<-AL.DT$loc
      names<- AL.DT$attr
      displayName<-gsub("[-:]",".",names)      
      attrLinkItems<-structure( paste0("\\link[=", dest,"]{",displayName,"}"), names=names)      
    } else {
      attrLinkItems<-c()
    }
    attrLinkItems
  }
  
  getAttrInGrp<-function(category){
    #category<-sub("s$","",category)
    aCS.DT[name==category]$value->attrs
    if(length(attrs)==0){
      attrs<-"Empty"
    } 
    attrs   
  }
  
  splitIntoGrps<-function(args, fn=getAttrInGrp){
    #process args
    if(length(args)==0){
      grp.list<-list( Empty= "No available content elements")
    } else {
      grpIndx<-grep(":$",args)
      specIndx<-setdiff(1:length(args), grpIndx)
      #grps<-args[grpIndx]
      spec.names<-args[specIndx]
      if(length(grpIndx)>0){ #we have some groups (i.e.  categories)
        grp.names<-args[grpIndx]
        #for elemGrps remove final colon  and captilize 
        grp.names<-sub(":$","",grp.names)       
        grp.list<-lapply(grp.names, fn) #!!!
        grp.names<-capitalizeIt(grp.names)
        names(grp.list)<-grp.names
        grp.names<-sort(grp.names)
        
        #elemGrps<-sub(" ","-",elemGrps)
      } else { #no groups
        grp.names<-c()
        grp.list<-list()
        spec.names<-args
      }     
      if(length(spec.names)>0){
        #spec.names<-sort(spec.names)
        grp.list<-c(grp.list, list("Other Elements:"= spec.names))
        grp.names<-c(grp.names,"Other Elements:")
      }
    }
    grp.list<-lapply(grp.list, function(x) sort(gsub("[-:]",".", x)))
      #gsub("[-:]",".",x))
    grp.list<-lapply(grp.names, function(x){ paste0("\\code{\\link{", grp.list[[x]], "}}")})
    names(grp.list)<-grp.names
    grp.list
  }
  
  
  #helper fn to write doc for single element
  addElementEntry<-function(elName){#, 
    #content.elements
    elemArgs<-es.DT$value[ content.DT[element==elName]$content[[1]] ]
    elemArgs<-expand.arg.names(elemArgs)
    description="Need to be written!!!"
      
    #process elemArgs
    showMe(elName)
 
    
    elemCats<-extract.CatMember.List(elemArgs, 
                           other="Other Elements:")
    

  elemCats<-lapply(elemCats, function(x) gsub("[-:]",".", x))
  elemCats<-lapply(elemCats, function(x)paste0("\\code{\\link{",x,"}}" ) )
  elemArgsItems<-lapply(names(elemCats),function(category){
      paste(
        "\\subsection{",
        category,
        "}{",
        paste(elemCats[[category]],collapse=", "),
        "}",
        sep="",
        collapse=", "
      )
    })
    unlist(elemArgsItems)->elemArgsItems
    #elemArgsItems<-paste0("\\item{ \\code{\\link{", elemArgs, "}}}")
    #---end content element handlers------------------------------------
    
    #---begin attribute  handlers---------------------------------------
    #use es.DT to get attrLoc and attrNames for that element,
      
    attrArgs<-es.DT[ element==elName & variable=="attr"]$value    
    attrArgs<-expand.arg.names(attrArgs)

    #attrArgs<-splitIntoGrps(args=attrArgs, fn=getAttrInGrp)
    attrCats<-extract.CatMember.List(attrArgs, 
                                 other="Other Attributes:")

    #For each attrArg create a single rd item based upon loctation and name
    attrLinkItems<- makeAttrLinkItems(attrArgs, elName)
 
    #attrCats<-lapply(names(attrCats), function(x)paste0("\\code{\\link{",x,"}}" ) )

    attrArgsItems<-lapply(names(attrCats),function(category){     
      attr.cat.members<-attrCats[[category]]
      #attrs.location<-attrLoc[attr.names]
      attr.links.in.cat<-attrLinkItems[attr.cat.members]
      paste(
        "\\item{",
        category,
        "}{",
        paste(attr.links.in.cat, collapse=", "),
        "}",
        sep="",
        collapse=", "
      )
    })
    unlist(attrArgsItems)->attrArgsItems
    

#     attrArgsItems<-sapply(names(attrCats),function(n){
#       paste0("\\item{ ", n,"}{", paste(attrArgs[[n]],collapse=", "), "}")
#     })


    #attrArgsItems<-paste0("\\item{ ", attrArgs, "}")  
    # if ends with colon is group 
    # to find attrs in group must remove colon
    # 1. split into group and specific
    # 2. populate grp 
    # 3. populate spec
    # 4. reformat attrs having - and colons
    # 5. add custom attrs
    # 6. 
    
    #---begin attribute  handlers---------------------------------------
    # if ends with a colon, find the attribute class
    name<-gsub("[-:]",".",elName)
    txt<-c(
      paste("@name", name), #
      paste("@title", name), #todo!!! add something meaningfull??
      "@description ",
      description,
      "@section Available Content Elements:",
      #"\\describe{",
      elemArgsItems,
      #"}",
      "@section Available Attributes:",
      "\\describe{",
      attrArgsItems,
      "}",
      "@keywords internal"
    )
    tmp<-paste("#' ", txt, sep="", collapse="\n")
    tmp
  }
    

  #vector of all elements
  unique(es.DT$element)->all.elements
  # content.DT
  es.DT[variable=="content.model", list(content=list(I(.I))), by=element]->content.DT
  es.DT[variable=="attr",  list(attr=list(I(.I))), by=element]->attributes.DT

  
  eleL<-lapply( all.elements, addElementEntry)
  rtv<-paste(eleL, "\nNULL\n", collapse="\n")
}


generate.Reg.Attr.Pages<-function(){
  #requries AVD.DT, AVE.DT
  addAttributeEntry<-function(alink){
    #showMe(alink)
    tmp1.DT<-AVEL.DT[loc==alink]
    elements<-tmp1.DT$element
    anim<-unique(tmp.DT$anim) #works since there is at most 1
    tmp2.DT<-AVD.DT[loc==alink]
    values<-AVD.DT[loc==alink]$value
    valDes<-AVD.DT[loc==alink]$value.def
    #showMe(valDes)
    #valDes<-gsub( "(@[-\\w:]+)" ,"\\1 attribute", valDes, perl=T)
    valDes<-gsub('[-:]',".",valDes)
    valDes<-gsub('@','',valDes)
    
    title<-unique(AVEL.DT[loc==alink]$attr )
    elements<-paste("\\code{\\link{", elements, "}}", sep="", collapse=", ")
    txt<-c(
      paste("@name", alink),
      paste("@title",title), 
      paste("@section Available Attribute Values:"),     
      paste("\\describe{"),
      paste("\\item{ ",   values, "}{", valDes,"}", sep=""),
      "}",
      paste("@section Used by the Elements:"),           
      paste("\\itemize{"),
      paste("\\item{ ",   elements, "}", sep=""),
      "}",
      "@keywords internal"
    )
    tmp<-paste("#' ", txt, sep="", collapse="\n")  
  }
  links<-unique(AVEL.DT$loc)
  attrDefsPages.List<-lapply( links, addAttributeEntry)
 
  rtv<-paste(attrDefsPages.List, "\nNULL\n", collapse="\n")
  #need 1. loc (becomes @Name)
  #     2. AttrName = @title
  #     3. Attr Values
  #     4. Attr Values Des
  #     5. Elements which attr applies to
  #     6. Animatable??
  #     7. related attrs??
  #     

  
}

# requires es.DT, AVEL.DT, AVD.DT,
do.documentation<-function(es.DT, composerFiles="composerFiles"){ 
#listing of all elements
#   eleAlphabeticalIndexDoc<-gen.all.Elem.Index(es.DT) 
#   cat(eleAlphabeticalIndexDoc, file=paste(composerFiles, "eleAlphabeticalIndexDoc.R", sep="/"))
  
  #listing of Element by Categories
  ele.cat.indx<-generate.ele.cat.Index()
  cat( ele.cat.indx, file=paste(composerFiles, " ele.cat.indx.page.R", sep="/") )
  
  #elecat doc
#   elemCatDoc<-get.Elem.categories(es.DT)
#   cat(elemCatDoc, file=paste(composerFiles, "elemCatDoc.R", sep="/") )
  
  #individual element documentation
  ele.pages<-generate.element.pages()
  cat(ele.pages, file=paste(composerFiles, "ele.pages.doc.R", sep="/") )

  regAttrPagesDoc<-generate.Reg.Attr.Pages()
  cat(regAttrPagesDoc, file=paste(composerFiles, "regAttr.pages.doc.R", sep="/") )
  #attr doc
  #attrDefDoc<-get.Attr.defs(es.DT)
  #cat(attrDefDoc, file=paste(composerFiles, "attrDefDoc.R", sep="/") ) 
}

#do.documentation(es.DT)
