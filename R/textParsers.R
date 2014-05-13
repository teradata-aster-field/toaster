#' Token parser generates n-word combinations as text terms.
#' 
#' @param n number of words 
#' @export
token <- function(n, ignoreCase = FALSE, delimiter = NULL, punctuation = NULL,
                  where = NULL) {
  
  stopifnot(n > 0)
  
  z <- structure(list(method = "token",
                      n = n
  ),
  class = c("token"))
  
  z
}

#' Ngram parser generates n-grams as text terms.
#' 
#' @param n length of ngrams to parse
#' @export
ngram <- function(n, delimeter = NULL, overlapping = TRUE, ignoreCase = FALSE, 
                  punctuation = NULL, reset = NULL, where = NULL) {
  
  stopifnot(n > 0)
  
  z <- structure(list(method = "ngram",
                      n = n,
                      delimiter = delimeter,
                      overlapping = overlapping,
                      ignoreCase = ignoreCase,
                      punctuation = punctuation,
                      reset = reset
  ),
  class = c("ngram"))
  
  z
  
}

textParserSQL <- function(x, tableName, docId, textColumns, where) {
  UseMethod("textParserSQL")
}

textParserSQL.ngram <- function(x, tableName, docId, textColumns, where) {
  
  where_clause = makeWhereClause(where)
  
  selectList = makeTextSelectList(docId, textColumns)
  
  sql = paste0(
    "SELECT ", docId, " docid, ngram as term, frequency 
       FROM nGram (
         ON (SELECT ", selectList, " FROM ", tableName,  where_clause, " ) ",
   "     TEXT_COLUMN('__text_column__')  ",
   ifelse(is.null(x$delimiter), " ", paste0(" DELIMITER('", x$delimiter,"') ")),     
   "     GRAMS(", as.character(x$n), ") 
         OVERLAPPING('", ifelse(x$overlapping, "true", "false"), "')
         CASE_INSENSITIVE('", ifelse(x$ignoreCase, "true", "false"), "') ",
   ifelse(is.null(x$punctuation), " ", paste0(" PUNCTUATION('", x$punctuation, "') ")),
   ifelse(is.null(x$reset), " ", paste0(" RESET('", x$reset, "') ")), " 
         ACCUMULATE('", docId, "')
       ) "
  )
}

textParserSQL.token <- function(x, tableName, docId, textColumns, where) {
  
  where_clause = makeWhereClause(where)
  
  selectList = makeTextSelectList(docId, textColumns)
  
}

makeTextSelectList <- function(docId, textColumns) {
  
  textExpr = paste(textColumns, collapse=" || ' ' || ")
  selectList = paste0(docId, ", ", textExpr, " __text_column__ ")
}