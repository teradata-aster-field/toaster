
# docs <- c("This is a text.", "This another one.")
# 
# corps = Corpus(VectorSource(docs))
# 
# tdm = TermDocumentMatrix(corps)
# dtm = DocumentTermMatrix(corps)

#' Compute term frequencies on a corpus.
#' 
#' @param channel connection object as returned by \code{\link{odbcConnect}}
#' @param tableName Aster table name
#' @param docId vector with one or more column names comprising unique document id. 
#'   Values are concatenated with \code{idSep}. Database NULLs are replaced with
#'   \code{idNull} string.
#' @param textColumns one or more names of columns with text. Multiple coumn are
#'   concatenated into single text field first.
#' @param parser type of parser to use on text. For example, \code{ngram(2)} parser
#'   generates 2-grams (ngrams of length 2), \code{token(2)} parser generates 2-word 
#'   combinations of terms within documents.
#' @param weighting term frequency formula to compute the tf value. One of following: 
#'   \code{'raw'}, \code{'bool'}, \code{'binary'}, \code{'log'}, \code{'augment'}, and
#'   \code{'normal'} (default). 
#' @param idSep separator when concatenating 2 or more document id columns (see \code{docId}).
#' @param idNull string to replace NULL value in document id columns.
#' @param where specifies criteria to satisfy by the table rows before applying
#'   computation. The criteria are expressed in the form of SQL predicates (inside 
#'   \code{WHERE} clause).
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \link{RODBC} 
#'   functions \link{sqlQuery} and \link{sqlSave}). 
#' @seealso \code{\link{nGram}}, \code{\link{token}}
#' @export 
computeTf <- function(channel, tableName, docId, textColumns, parser,
                      weighting = "normal",
                      where = NULL, idSep = '-', idNull = '(null)',
                      test = FALSE) {
  
  weighting = match.arg(weighting, c('raw','bool','binary','log','augment','normal'))
  tfFormula = switch(weighting,
                    normal="normal",
                    raw="normal",
                    bool="bool",
                    binary="bool",
                    log="log",
                    augment="augment",
  )
  
  where_clause = makeWhereClause(where)
  
  derivedDocId = makeDocumentId(docId, idSep, idNull)
  
  textSelectSQL = parseTextSQL(parser, tableName, derivedDocId, textColumns, where)
  
  sql = paste0(
    "SELECT * FROM TF(
         ON (SELECT docid, term FROM ( ", textSelectSQL, " ) t ) PARTITION BY docid
         FORMULA('", tfFormula, "')
       )")
  
  if (test) 
    return (sql)
  else {
    rs = toaSqlQuery(channel, sql, stringsAsFactors = FALSE)
  }
  
  x = makeSimpleTripletMatrix(rs, ifelse(weighting == 'raw', 'count', 'tf'), 'tf')
  return(x)
}

#' Compute Term Frequency - Inverse Document Frequency on a corpus.
#' 
#' @param channel connection object as returned by \code{\link{odbcConnect}}
#' @param tableName Aster table name
#' @param docId vector with one or more column names comprising unique document id. 
#'   Values are concatenated with \code{idSep}. Database NULLs are replaced with
#'   \code{idNull} string.
#' @param textColumns one or more names of columns with text. Multiple coumn are
#'   concatenated into single text field first.
#' @param parser type of parser to use on text. For example, \code{ngram(2)} parser
#'   generates 2-grams (ngrams of length 2), \code{token(2)} parser generates 2-word 
#'   combinations of terms within documents.
#' @param idSep separator when concatenating 2 or more document id columns (see \code{docId}).
#' @param idNull string to replace NULL value in document id columns.
#' @param adjustDocumentCount logical: if TRUE then number of documents 2 will be increased by 1.
#' @param where specifies criteria to satisfy by the table rows before applying
#'   computation. The criteria are expressed in the form of SQL predicates (inside 
#'   \code{WHERE} clause).
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \link{RODBC} 
#'   functions \link{sqlQuery} and \link{sqlSave}). 
#' @seealso \code{\link{nGram}}, \code{\link{token}}
#' @export 
computeTfIdf <- function(channel, tableName, docId, textColumns, parser, 
                         idSep = '-', idNull = '(null)',
                         adjustDocumentCount = FALSE, where = NULL, 
                         test = FALSE) {
  
  where_clause = makeWhereClause(where)
  
  derivedDocId = makeDocumentId(docId, idSep, idNull)
  
  # validate number of documents > 1 for TF-IDF
  # and adjust 2 to 3 if requested
  if (!test) {
    countSql = paste0("SELECT COUNT(DISTINCT(", derivedDocId, ")) count ", " FROM ", tableName, where_clause)
    docCount = toaSqlQuery(channel, countSql)$count[[1]]
    if (docCount < 2)
      stop("Can't compute TF-IDF for single document. Use 'computeTf` that computes term frequency instead.")
    
    # adjust for 2 documents 
    increaseByOne = ifelse(adjustDocumentCount && docCount == 2, " + 1 ", " ")
  }else
    increaseByOne = " "
  
  textSelectSQL = parseTextSQL(parser, tableName, derivedDocId, textColumns, where)
  
  sql = paste0(
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term FROM ( ", textSelectSQL, " ) t ) PARTITION BY docid
          ) AS TF PARTITION BY term
       ON ( SELECT COUNT(DISTINCT(", derivedDocId, ")) ", increaseByOne," FROM ", tableName, where_clause, " )
            AS doccount dimension
     )")
  
  if (test) 
    return (sql)
  else {
    rs = toaSqlQuery(channel, sql, stringsAsFactors = FALSE)
  }
  
  x = makeSimpleTripletMatrix(rs, 'tf_idf', "ti")
  return (x)
}


makeDocumentId <- function(docId, idSep, idNull) {
  
  collapse = paste0(" || '", idSep, "' || ")
  derivedId = paste0("COALESCE(CAST(", docId, " AS varchar), '", idNull, "')", collapse = collapse)
  
}

TermDocumentMatrix_classes <-
  c("toaTermDocumentMatrix", "TermDocumentMatrix", "simple_triplet_matrix")

makeSimpleTripletMatrix <- function(result_set, weight_name, weighting = "tf") {
  
  terms = result_set$term
  docs = result_set$docid
  weights = result_set[, weight_name]
  
  allTerms = sort(unique(terms))
  allDocs = sort(unique(docs))
  i = match(terms, allTerms)
  j = match(docs, allDocs)
  
  m = simple_triplet_matrix(i = i, j = j, v = as.numeric(weights),
                            nrow = length(allTerms),
                            ncol = length(allDocs),
                            dimnames =
                              list(Terms = allTerms,
                                   Docs = allDocs))
  
  m$rs = result_set
  class(m) <- TermDocumentMatrix_classes
  attr(m, "Weighting") <- weighting
  
  return(m)
}