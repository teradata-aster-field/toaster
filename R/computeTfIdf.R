
# docs <- c("This is a text.", "This another one.")
# 
# corps = Corpus(VectorSource(docs))
# 
# tdm = TermDocumentMatrix(corps)
# dtm = DocumentTermMatrix(corps)

#' Compute TF-IDF on corpus of documents.
#' 
#' @param channel connection object as returned by \code{\link{odbcConnect}}
#' @param tableName Aster table name
#' @param docId name of the column with document id
#' @param textColumns one or more names of columns with text. Multiple coumn are
#'   concatenated into single text field first.
#' @param parser type of parser to use on text. For example, \code{ngram(2)} parser
#'   generates ngrams of length 2, \code{token(2)} parser generates 2-word combination
#'   terms.
#' @param where specifies criteria to satisfy by the table rows before applying
#'   computation. The creteria are expressed in the form of SQL predicates (inside
#'   \code{WHERE} clause).
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \link{RODBC} 
#'   functions like \link{sqlQuery} and \link{sqlSave}). 
#' @seealso \code{\link{ngram}}, \code{\link{token}}
#' @export 
computeTfIdf <- function(channel, tableName, docId, textColumns, parser, 
                         where = NULL, test = FALSE) {
  
  textSelectSQL = textParserSQL(parser, tableName, docId, textColumns, where)
  
  where_clause = makeWhereClause(where)
  
  sql = paste0(
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term FROM (", textSelectSQL, ") t) PARTITION BY docid
          ) AS TF PARTITION BY term
       ON ( SELECT COUNT(DISTINCT(", docId, ")) FROM ", tableName, where_clause, " )
            AS doccount dimension
     )")
  
  if (test) 
    return (sql)
  else {
    sqlQuery(channel, sql, stringsAsFactors = FALSE)
  }
}


