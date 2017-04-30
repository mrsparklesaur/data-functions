# Make a list with each citation in a separate sub-list
read.refs<-function(filelist){
  allreflist<-list()
  block<-0
  nrow<-0
  while(block <= 257){
    indreflist<-list()
    row<-nrow + 1
    while (grepl("ER", filelist[row,1])==FALSE){
      indreflist[[row]]<-as.character(dat[row,])
      indreflist<-indreflist[!sapply(indreflist, is.null)]
      row <- row +1
    }
    allreflist[[block + 1]]<-indreflist
    block <- block + 1
    nrow<-row 
  }
  return(allreflist)
}

# create dataframe with information from each file
get.citation<-function(refdatabase){
  df<-data.frame()
  for (i in 1:length(refdatabase)){
    reflist<-refdatabase[[i]]
    authors1<-find.authors(reflist)
    authors2<-paste(authors1, collapse=", ")
    year<-find.year(reflist)
    journal<-find.journal(reflist)
    title<-find.title(reflist)
    volume<-find.volume(reflist)
    pages<-find.pages(reflist)
    df<-rbind(df, data.frame(authors2, year, title, journal, volume, pages))
  }
  return(df)
}

# extract authors
find.authors<-function(reflist){
  authors<-grep("AU", reflist)
  authorlist<-vector()
  for (i in authors){
    author<-trimws(sub("AU  -","",reflist[[i]]), which = "left")
    authorlist<-append(authorlist, author)
  }
  return(authorlist)
}

# extract title
find.title<-function(reflist){
  if (any(grepl("TI",reflist))){
    titles<-grep("TI", reflist)[[1]]
    title<-trimws(sub("TI  -","",reflist[[titles]]), which = "left")  
  } else {
    title<-NA
  }
  return(title)
}

# extract year
find.year<-function(reflist){
  if (any(grepl("PY",reflist))){
    years<-grep("PY", reflist)[[1]]
    year<-trimws(sub("PY  -","", reflist[[years]]), which = "left")  
  } else {
    year<-NA
  }
  return(as.numeric(year))
}

# extract journal
find.journal<-function(reflist){
  if (any(grepl("T2",reflist))){
    journals<-grep("T2", reflist)[[1]]
    journal<-trimws(sub("T2  -","", reflist[[journals]]), which = "left")
  } else {
    journal<-NA
  }
  return(journal)
}

# extract volume
find.volume<-function(reflist){
  if (any(grepl("VL",reflist))){
    vols<-grep("VL", reflist)[[1]]
    vol<-trimws(sub("VL  -","",reflist[[vols]]), which = "left")  
  } else {
    vol<-NA
  }
  return(as.numeric(vol))
}

# extract page numbers
find.pages<-function(reflist){
  if (any(grepl("SP",reflist))){
    pages<-grep("SP", reflist)[[1]]
    page<-trimws(sub("SP  -","",reflist[[pages]]), which="left")
  } else {
    page<-NA
  }
  return(page)
}

# Author list for each paper
author.list<-function(refdatabase){
  authorlist<-data.frame()
  for (i in 1:length(refdatabase)){
    reflist<-refdatabase[[i]]
    authors<-find.authors(reflist)
    studyN<-rep(i, times=length(authors))
    authorlist<-rbind(authorlist, data.frame(studyN,authors))
  }
  return(authorlist)
}

