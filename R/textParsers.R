#' Tokenize (or split) text and emit n-word combinations from a document.
#' 
#' When n=1 simply tokenize text and emit words with counts. When n>1
#' tokenized words are combined into permutations of length n within
#' each document.
#' 
#' @param n number of words 
#' @export
token <- function(n, tokenSep = '+', ignoreCase = FALSE, delimiter = '[ \\t\\b\\f\\r]+', punctuation = NULL,
                  stemming = FALSE, stopWordsFile = NULL, sep = " ") {
  
  stopifnot(n > 0)
  
  z <- structure(list(method = "token",
                      n = n,
                      tokenSep = tokenSep,
                      ignoreCase = ignoreCase,
                      delimiter = delimiter,
                      punctuation = punctuation,
                      stemming = stemming,
                      stopwords = stopWordsFile,
                      sep = sep
  ),
  class = c("token", "toatextparser"))
  
  z
}

#' Tokenize (or split) text and emit n multi-grams.
#' 
#' 
#' @param n length, in words, of each n-gram
#' @param delimiter character or string that divides one word from the next. 
#'   You can use a regular expression as the \code{delimiter} value.
#' @param overlapping logical: true value allows for overlapping n-grams.
#' @param ignoreCase logical: if FALSE, the n-gram matching is case sensitive and
#'   if TRUE, case is ignored during matching.
#' @param punctuation a regular expression that specifies the punctuation characters 
#'   parser will remove before it evaluates the input text.
#' @param reset a regular expression listing one or more punctuation characters or 
#'   strings, any of which the \code{nGram} parser will recognize as the end of a sentence 
#'   of text. The end of each sentence resets the search for n-grams, meaning that 
#'   \code{nGram} discards any partial n-grams and proceeds to the next sentence to search 
#'   for the next n-gram. In other words, no n-gram can span two sentences.
#' @param sep a character string to separate multiple text columns.
#' 
#' @export
nGram <- function(n, delimiter = NULL, overlapping = TRUE, ignoreCase = FALSE, 
                  punctuation = NULL, reset = NULL, sep = " ") {
  
  stopifnot(n > 0)
  
  z <- structure(list(method = "ngram",
                      n = n,
                      delimiter = delimiter,
                      overlapping = overlapping,
                      ignoreCase = ignoreCase,
                      punctuation = punctuation,
                      reset = reset,
                      sep = sep
  ),
  class = c("nGram", "toatextparser"))
  
  z
  
}

parseTextSQL <- function(x, tableName, docId, textColumns, where) {
  UseMethod("parseTextSQL")
}

parseTextSQL.nGram <- function(x, tableName, docId, textColumns, where) {
  
  where_clause = makeWhereClause(where)
  
  selectList = makeTextSelectList(docId, textColumns, x$sep)
  
  sql = buildNGramSQL(x, x$n[[1]], tableName, docId, selectList, where_clause)
  for (n in x$n[-1]) {
    sql = paste0(
      sql, " UNION ALL ", buildNGramSQL(x, n, tableName, docId, selectList, where_clause))
  }
  
  return (sql)
}

buildNGramSQL <- function (x, n, tableName, docId, selectList, where_clause) {
  sql = paste0(
    "SELECT ", docId, " docid, ngram as term, frequency 
         FROM nGram (
           ON (SELECT ", selectList, " FROM ", tableName, where_clause, ")
           TEXT_COLUMN('__text_column__') ",
    ifelse(is.null(x$delimiter), " ", paste0(" DELIMITER('", x$delimiter,"') ")), "
           GRAMS(", as.character(n), ") 
           OVERLAPPING('", ifelse(x$overlapping, "true", "false"), "')
           CASE_INSENSITIVE('", ifelse(x$ignoreCase, "true", "false"), "') ",
    ifelse(is.null(x$punctuation), " ", paste0(" PUNCTUATION('", x$punctuation, "') ")),
    ifelse(is.null(x$reset), " ", paste0(" RESET('", x$reset, "') ")), " 
           ACCUMULATE('", docId, "')
         ) "
  )
}

parseTextSQL.token <- function(x, tableName, docId, textColumns, where) {
  
  where_clause = makeWhereClause(where)
  
  selectList = makeTextSelectList(docId, textColumns, x$sep)
  withTable = "tokens"
  withSelect = paste0(
    "SELECT *
       FROM text_parser(
         ON (SELECT ", selectList, " FROM ", tableName, where_clause, " ) 
         PARTITION BY ", docId, " 
         TEXT_COLUMN('__text_column__') ",
    ifelse(is.null(x$delimiter), " ", paste0(" DELIMITER('", x$delimiter,"') ")), " 
         CASE_INSENSITIVE('", ifelse(x$ignoreCase, "true", "false"), "')
         STEMMING('", ifelse(x$stemming, "true", "false"), "') ",
    ifelse(is.null(x$punctuation), " ", paste0(" PUNCTUATION('", x$punctuation, "') ")),
    ifelse(is.null(x$stopwords), " ", paste0(" STOP_WORDS('", x$stopwords, "') ")), "
         ACCUMULATE('", docId, "')
         TOTAL('false')
         LIST_POSITIONS('false')
         TOKEN_COLUMN_NAME('term')
         FREQUENCY_COLUMN_NAME('frequency')
         OUTPUT_BY_WORD('true')
       )
      WHERE length(term) > 0"
  )
  
  aliases = paste0(rep("t", x$n), as.character(seq(1, x$n)))
  if (x$n > 1) {
    collapse = paste0(" || '", x$tokenSep, "' || ")
    termExpr = paste0(aliases, ".term", collapse=collapse)
    fromTables = paste0(withTable, " ", aliases[[1]])
    for (i in 2:x$n) {
      leftt = aliases[[i-1]]
      rightt = aliases[[i]]
      fromTables = paste0(fromTables, " JOIN ", withTable, " ", rightt,
                             " ON (", leftt, ".", docId, " = ", rightt, ".", docId, " AND 
                                  ",  leftt, ".term < ", rightt, ".term)")
    }
  }else {
    termExpr = "term"
    fromTables = paste0(withTable, " ", aliases)
  }
  
  
  
  sql = paste0(
    "WITH ", withTable, " AS 
       (", withSelect, ")  
     SELECT ", aliases[[1]], ".", docId, " docid, ", termExpr, " term, COUNT(*) frequency
       FROM ", fromTables, "
      GROUP BY 1, 2")
  
}

makeTextSelectList <- function(docId, textColumns, sep) {
  
  collapse = paste(" || '", sep, "' || ")
  textExpr = paste(textColumns, collapse=collapse)
  selectList = paste0(docId, ", ", textExpr, " __text_column__ ")
}