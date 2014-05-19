context("computeTfIdf")



test_that("computeTf SQL with nGram parser is correct", {
  
  expect_equal_normalized(
    computeTf(channel=NULL, tableName="texts.containertrailerplanpaths", docId="orig", 
              textColumns=c("commentsall", "commentsmissent", "drivernamemissent"),
              parser=nGram(2), test=TRUE),
    "SELECT * FROM TF(
         ON (SELECT docid, term 
               FROM ( SELECT __doc_id__ docid, ngram as term, frequency 
                    FROM nGram (
                      ON (SELECT COALESCE(CAST(orig AS varchar), '(null)') __doc_id__, commentsall || ' ' || commentsmissent || ' ' || drivernamemissent __text_column__  
                            FROM texts.containertrailerplanpaths )      
                      TEXT_COLUMN('__text_column__')        
                      GRAMS(2) 
                      OVERLAPPING('true')
                      CASE_INSENSITIVE('false')        
                      ACCUMULATE('__doc_id__')
                    ) 
               ) t
            ) PARTITION BY docid
         FORMULA('normal')
     )"
    )
})

test_that("computeTf SQL with token parser is correct", {
  
  expect_equal_normalized(
    computeTf(channel=NULL, tableName="texts.containertrailerplanpaths", docId="orig",
              textColumns=c("commentsall", "commentsmissent", "drivernamemissent"),
              parser=token(1, stopWordsFile="english.dat"), test=TRUE),
    "SELECT * FROM TF(
         ON (SELECT docid, term 
    FROM ( 
    WITH tokens AS
    (SELECT *
    FROM text_parser(
    ON (SELECT COALESCE(CAST(orig AS varchar), '(null)') __doc_id__, 
    commentsall || ' ' || commentsmissent || ' ' || drivernamemissent __text_column__ 
    FROM texts.containertrailerplanpaths )
    PARTITION BY __doc_id__ 
    TEXT_COLUMN('__text_column__')
    DELIMITER('[ \\t\\b\\f\\r]+')
    CASE_INSENSITIVE('false')
    STEMMING('false')
    STOP_WORDS('english.dat')
    ACCUMULATE('__doc_id__')
    TOTAL('false')
    LIST_POSITIONS('false')
    TOKEN_COLUMN_NAME('term')
    FREQUENCY_COLUMN_NAME('frequency')
    OUTPUT_BY_WORD('true')
    )
    WHERE length(term) > 0)
    SELECT t1.__doc_id__ docid, term term, COUNT(*) frequency
    FROM tokens t1
    GROUP BY 1, 2
    ) t
  ) PARTITION BY docid
  FORMULA('normal')
  )")
  
})