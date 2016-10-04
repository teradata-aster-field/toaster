
#' Normalize text to remove repeating white spaces across multiple lines
replaceWhite <- function(s) {
  gsub("\\s+", " ", s)
}

#' Extension of \code{\link{expect_equal}} to compare normalized texts
expect_equal_normalized <- function(object, expected, info = NULL, label = NULL) {
  
  expect_equal(replaceWhite(object), replaceWhite(expected), info=info, label=label)
  
}

#' Duplicate table info with new schema name
duplicateSchema <- function(tableInfo, newSchema='baseball') {
  
  tableInfo2 = tableInfo
  tableInfo2$TABLE_SCHEM = newSchema
  tableInfo22 = rbind(tableInfo, tableInfo2)
}



#' Compare two strings position by position and print differences
list.string.diff<-function(a="ATTCGA-",b="attTGTT",exclude=character(0),ignore.case=FALSE)
{
  if(ignore.case==TRUE)
  {
    a<-toupper(a)
    b<-toupper(b)
  }
  
  shorterLen = min(nchar(a), nchar(b))
  a = substr(a, 1, shorterLen)
  b = substr(b, 1, shorterLen)
  
  seq.a<-unlist(strsplit(a,split=""))
  seq.b<-unlist(strsplit(b,split=""))
  
  diff.d<-rbind(seq.a,seq.b)
  only.diff<-diff.d[,diff.d[1,]!=diff.d[2,]]
  pos<-which(diff.d[1,]!=diff.d[2,])
  only.diff<-rbind(pos,only.diff)
  for(ex.loop in 1:length(exclude))
  {
    only.diff<-only.diff[,!(only.diff["seq.a",]==exclude[ex.loop]|only.diff["seq.b",]==exclude[ex.loop])]
  }
  return(only.diff)
}