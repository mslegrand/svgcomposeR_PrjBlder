
#-----------------------------------------------------------------------------------------

#' Generates a Listing of all elements by category  (Index of Ele Cats)
generate.ele.cat.Index<-function(){ 
  oneCatListing<-function(category){
    #for a given category extracts the elements and
    #returns a section with category title and element listing
    es.DT[variable=="category" & value==category]$element->ele    
    sort(ele)->ele
    res<-c(
      rd.section( paste0(category,"s") ),
      rd.describe( rd.item( nameWithLink(ele) ) )
    )
  } 
   
  #categories are identified by es.DT
  cats<-unique(es.DT[variable=="category"]$value)
  cats<-sort(cats)
  # Element Group Name
  cat.index<-sapply(cats, oneCatListing)
  cat.index<-unlist(cat.index)
  cat.index<-c(
    cat.index,
    rd.name('Elememt Index'),
    rd.title('Element Generators Indexed by Category'),
    rd.description('This is a listing by category of generators to use when generating an svg markup.')
  ) 
  paste0(rd.close(cat.index), collapse="\n")
}

#-----------------------------------------------------------------------------------------



#' Splits a given vector of elements into list of categories with both 
#' list names (category) and members( elements for each cat) sorted
#' 
#'  USED ONLY BY: elements.by.category.listing
extract.CatMember.List<-function(members, other="Unclassifed"){
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
      tmp.DT<-data.table(rbind(tmp.DT, data.table(name=other, value=missing)))
    } 
    tmp.list<-structure(lapply(cats, function(kit)tmp.DT[name==kit]$value) ,names=cats)
  } 
  tmp.list
}

#' uses: extract.CatMember.List
#' used by: 
#'    generate.element.pages::addElementEntry, 
#'    generate.Pres.Attr.Pages 
elements.by.category.listing<-function( elemArgs ){ #USED IN 3 PLACES: 
  elemArgs<-sort(unique(elemArgs))
  elemCats<-extract.CatMember.List(elemArgs, other="Unclassfied:")   
    
  elemCats<-lapply(elemCats, function(x){
    if(any(grepl('Empty', x))| any(grepl('Any element',x))){
      rd.code(x)
    } else {
      nameWithLink(x)
    }
  })
  
  elemArgsItems<-lapply(names(elemCats),function(category){
        rd.item(rd.emph(capitalizeIt(category)), paste(elemCats[[category]],collapse=", "))
  })
  
  unlist(elemArgsItems)->elemArgsItems   
}

#' creates a location reference for a presentation attribute 
#' given the presentation attribute name 
getPresAttrsLoc<-function(presAttrs){ #USED 3 PLACES
  #gsub("[-:]",".",presAttrs)->presAttrs #remove the uglies
  presAttrsLoc<-paste0(presAttrs,"-presentationAttribute")
  presAttrsLoc
}


#' generates element documentation  for each element found in es.DT
#' That is creates all individual element pages
generate.element.pages<-function(){
  source('elementDescription.R')
  # ---------BEGIN HELPERS:generate.element.pages
  
  # replaces shape elements, descriptive elements, ... with the actual elements
  expand.content.ele.names<-function(arg.names){
    arg.names<-gsub(":$","",arg.names)
    names<-unique(eaCS.DT$name)
    ele.cat.names<-names[grep("elements$",names)] # names are the categories of ele according to eaCS
    indx.cat<-match(arg.names, ele.cat.names, nomatch=0L)
    arg.ele.no.cat<-arg.names[indx.cat==0]
    arg.ele.cat<-arg.names[indx.cat>0]
    arg.ele.cat<-lapply(arg.ele.cat, function(x)sort(eaCS.DT[name==x]$value) )
    c(unlist(arg.ele.cat),arg.ele.no.cat)
  }
  
  expand.content.ele.names.esDT<-function(ele.content){
    #pick out those ending with colon to be expanded  
    catIndx<-grep("elements:", ele.content)
    regArgs<-ele.content[-catIndx]
    catArgs<-ele.content[catIndx]
    #1 remove s and colon at end
    catArgs<-gsub("s:$","",catArgs)
    #2 capitalize 1 letter of each word
    catArgs<-capitalizeIt(catArgs)
    #3 extract the elements in these categories
    expansion<-es.DT[variable=='category' & value %in% catArgs]$element
    rtv<-c(expansion,regArgs)
    rtv
  }

  #--------
  requireTable(AVEL.DT)

  # input: elName, the name of an element
  # return: attr-link-items of all  attrs, 
  makeAttrLinkItems2<-function(elName){ #USED ONLY BY ELEMENT ADDENTRY
    
    #------regular attributes
    AL.DT<- AVEL.DT[element==elName, list(loc), key=attr]
    #setkey(AL.DT, attr)
    setkey(eaCS.DT, value) #do just once please!!!
    
    CAL.DT<-eaCS.DT[AL.DT]
    setnames(CAL.DT, c("category", "attr", "loc"))
    if(nrow(CAL.DT)>0){
      CAL.DT[is.na(category), category:='unclassified']
      CAL.DT[, attr:=gsub("[-:]", ".", attr)]
    } 
    
    #CAL.DT is a data.table of category, attribute location
    
    #-----combo attributes
    co.loc2<-function(attr,loc, variable){
      sapply(1:length(attr), function(i){
        pattern<-paste0(attr[i],"Attribute$")
        variable<-paste0(toupper(variable[i]),'Attribute')
        sub(pattern, variable, loc[i], ignore.case=T)    
      })   
    }
    
    # 1. get the combined attrs from COP.DT
    COP.DT[element==elName, .SD[1,], by=variable]->tmp1.DT      
    # 2. extract from AL.DT, the locations and form CAL.COP.DT for combined
    if(nrow(tmp1.DT)>0){
      setkey(tmp1.DT,value)
      #In one step :)
      CAL.COP.DT<-AL.DT[tmp1.DT,list(category='combining attributes', attr=variable, loc=co.loc2(attr, loc, variable))]
      setkey(CAL.COP.DT, attr) #make sure that it's sorted
      CAL.DT<-data.table(rbind(CAL.DT,CAL.COP.DT))        
    }
    
    #------presentation attributes
    presAttrs<-PA.DT[variable=="Applies to" & value==elName]$attr
    if(length(presAttrs)>0){
      #gsub("[-:]",".",presAttrs)->presAttrs #remove the uglies
      presAttrsLoc<-getPresAttrsLoc(presAttrs) #paste0("presAttrs.", presAttrs)      
      CAL.DT<-data.table(rbind(
        CAL.DT,
        data.table(category="presentation attributes", attr=presAttrs, loc=presAttrsLoc)
      ))  
    }
    
    
    # now put it all together
    if(nrow(CAL.DT)>0){
      setkey(CAL.DT, category, attr)
      CAL.LIST<-split(CAL.DT[, nameWithLink(attr, loc)] , CAL.DT$category)
       
      fn<-function(cat.name){
        rd.item(rd.emph(cat.name), paste0(CAL.LIST[[cat.name]], collapse=", ") )
      }
      
      attributesListing<-unlist(lapply(names(CAL.LIST), fn ))
    } else {
      attributesListing<-"{No Attributes Available}{!}"
    }
    attributesListing
  } #end of makeAttrLinkItems2
  
  #helper fn to write doc for single element
  addElementEntry<-function(elName){
    #showMe(elName)
    # ---content.element handeling--------------
      elemArgs<-es.DT[element==elName & variable=="content.model"]$value
      elemArgs<-expand.content.ele.names.esDT(elemArgs)
      # elemArgs are the content elements, i.e. elements that elName can take as parameters
      #elemArgs<-expand.content.ele.names(elemArgs) # replaces shape elements, ... with the actual elements 
    
    
      elemArgsItems<- elements.by.category.listing(elemArgs) # break up elements back into el-categoris, but now is list
    
    #--- attribute  handeling---------------------------------------      
      attrArgsItems<-makeAttrLinkItems2(elName)   #requries AVEL.DT
      
    #---pulling it together 
    #cat("elName=",elName,"\n")
    description<-element.decriptions[[elName]]["description"]
    title<-      element.decriptions[[elName]]["title"]
    if(is.null(description)){
      description<-"ToDo: Needs to be written!!!"
    }
    if(is.null(title)){
      title<-paste0("ala ",asDot(elName))
    }
    
    
    txt<-c(
      rd.name(elName), 
      #rd.title(asDot(elName)), 
      rd.title(title), 
      if(asDot(elName)!=elName){
        rd.aliases(asDot(elName))
      } else{
        NULL
      },
      rd.description(description),
      
      rd.section("Available Attributes (Named Parameters)" ),
      rd.describe( attrArgsItems ), 
      if(length(elemArgsItems)>0){
        c(
          rd.section("Available Content Elements (Unnamed Parameters)" ),
          rd.describe( elemArgsItems )
        ) 
      } else {NULL},
      rd.keywords("element")
    )
    txt<-paste0(rd.close(txt),collapse="\n")
    txt

  } #END: addElementEntry
  
  # ---------END HELPERS:generate.element.pages
  
  #vector of all elements
  unique(es.DT$element)->all.elements
  # content.DT
  #es.DT[variable=="content.model", list(content=list(I(.I))), by=element]->content.DT
  es.DT[variable=="content.model",list(element,value)]->content2.DT

  es.DT[variable=="attr",  list(attr=list(I(.I))), by=element]->attributes.DT
  
  eleL<-lapply( all.elements, addElementEntry)
  rtv<-paste(eleL,collapse="\n")
  
} 
#----------- END: generate.element.pages
