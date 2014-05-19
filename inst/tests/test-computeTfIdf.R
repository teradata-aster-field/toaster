context("computeTfIdf")



test_that("computeTfIdf SQL with nGram parser is correct", {
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="texts.containertrailerplanpaths", docId="orig", 
                 textColumns=c("commentsall", "commentsmissent", "drivernamemissent"),
                 parser=nGram(2), test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
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
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(COALESCE(CAST(orig AS varchar), '(null)'))) FROM texts.containertrailerplanpaths )
            AS doccount dimension
     )"
    )
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="texts.containertrailerplanpaths", docId=c("orig", "firststc"), 
                 idSep = '***', idNull = '<n-u-l-l>',
                 textColumns=c("commentsall", "commentsmissent", "drivernamemissent"),
                 parser=nGram(2), test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( SELECT __doc_id__ docid, ngram as term, frequency 
                    FROM nGram (
                      ON (SELECT COALESCE(CAST(orig AS varchar), '<n-u-l-l>') || '***' || COALESCE(CAST(firststc AS varchar), '<n-u-l-l>') __doc_id__, 
                                 commentsall || ' ' || commentsmissent || ' ' || drivernamemissent __text_column__  
                            FROM texts.containertrailerplanpaths )      
                      TEXT_COLUMN('__text_column__')        
                      GRAMS(2) 
                      OVERLAPPING('true')
                      CASE_INSENSITIVE('false')        
                      ACCUMULATE('__doc_id__')
                    ) 
               ) t
            ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(COALESCE(CAST(orig AS varchar), '<n-u-l-l>') || '***' || COALESCE(CAST(firststc AS varchar), '<n-u-l-l>'))) FROM texts.containertrailerplanpaths )
            AS doccount dimension
     )"
  )
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="texts.containertrailerplanpaths", docId="orig",
                 textColumns=c("commentsall"), 
                 parser=nGram(2), where="orig IN ('75H', '010PM')", test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( SELECT __doc_id__ docid, ngram as term, frequency 
                    FROM nGram (
                      ON (SELECT COALESCE(CAST(orig AS varchar), '(null)') __doc_id__, commentsall __text_column__  
                            FROM texts.containertrailerplanpaths WHERE orig IN ('75H', '010PM') ) 
                      TEXT_COLUMN('__text_column__')        
                      GRAMS(2) 
                      OVERLAPPING('true')
                      CASE_INSENSITIVE('false')   
                      ACCUMULATE('__doc_id__')
                    ) 
               ) t
            ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(COALESCE(CAST(orig AS varchar), '(null)'))) FROM texts.containertrailerplanpaths WHERE orig IN ('75H', '010PM') )
            AS doccount dimension
     )"
    )
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="texts.containertrailerplanpaths", docId="orig", 
                 textColumns=c("commentsall", "commentsmissent", "drivernamemissent"),
                 parser=nGram(2:4), test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( SELECT __doc_id__ docid, ngram as term, frequency 
                        FROM nGram (
                          ON (SELECT COALESCE(CAST(orig AS varchar), '(null)') __doc_id__, 
                                     commentsall || ' ' || commentsmissent || ' ' || drivernamemissent __text_column__  
                                FROM texts.containertrailerplanpaths )      
                          TEXT_COLUMN('__text_column__')        
                          GRAMS(2) 
                          OVERLAPPING('true')
                          CASE_INSENSITIVE('false')        
                          ACCUMULATE('__doc_id__')
                    ) 
                    UNION ALL
                      SELECT __doc_id__ docid, ngram as term, frequency 
                        FROM nGram (
                          ON (SELECT COALESCE(CAST(orig AS varchar), '(null)') __doc_id__, 
                                     commentsall || ' ' || commentsmissent || ' ' || drivernamemissent __text_column__  
                                FROM texts.containertrailerplanpaths )      
                          TEXT_COLUMN('__text_column__')        
                          GRAMS(3) 
                          OVERLAPPING('true')
                          CASE_INSENSITIVE('false')        
                          ACCUMULATE('__doc_id__')
                    )
                    UNION ALL
                      SELECT __doc_id__ docid, ngram as term, frequency 
                        FROM nGram (
                          ON (SELECT COALESCE(CAST(orig AS varchar), '(null)') __doc_id__, 
                                     commentsall || ' ' || commentsmissent || ' ' || drivernamemissent __text_column__  
                                FROM texts.containertrailerplanpaths )      
                          TEXT_COLUMN('__text_column__')        
                          GRAMS(4) 
                          OVERLAPPING('true')
                          CASE_INSENSITIVE('false')        
                          ACCUMULATE('__doc_id__')
                   )
                 ) t
            ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(COALESCE(CAST(orig AS varchar), '(null)'))) FROM texts.containertrailerplanpaths )
            AS doccount dimension
     )"
  )
})

test_that("computeTfIdf SQL with token parser is correct", {
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="texts.containertrailerplanpaths", docId="orig", 
                 textColumns=c("commentsall", "commentsmissent", "drivernamemissent"),
                 parser=token(1, stopWordsFile="english.dat"), test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
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
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(COALESCE(CAST(orig AS varchar), '(null)'))) FROM texts.containertrailerplanpaths )
            AS doccount dimension
     )")
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="texts.containertrailerplanpaths", docId="orig", 
                 textColumns=c("commentsall", "commentsmissent", "drivernamemissent"),
                 parser=token(2), where="orig IN ('75H', '010PM')", test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( 
                 WITH tokens AS
                   (SELECT *
                      FROM text_parser(
                        ON (SELECT COALESCE(CAST(orig AS varchar), '(null)') __doc_id__, commentsall || ' ' || commentsmissent || ' ' || drivernamemissent __text_column__ 
                              FROM texts.containertrailerplanpaths WHERE orig IN ('75H', '010PM') )
                        PARTITION BY __doc_id__ 
                        TEXT_COLUMN('__text_column__')
                        DELIMITER('[ \\t\\b\\f\\r]+')
                        CASE_INSENSITIVE('false')
                        STEMMING('false')
                        ACCUMULATE('__doc_id__')
                        TOTAL('false')
                        LIST_POSITIONS('false')
                        TOKEN_COLUMN_NAME('term')
                        FREQUENCY_COLUMN_NAME('frequency')
                        OUTPUT_BY_WORD('true')
                      )
                    WHERE length(term) > 0)
                SELECT t1.__doc_id__ docid, t1.term || '+' || t2.term term, COUNT(*) frequency
                  FROM tokens t1 JOIN 
                       tokens t2 ON (t1.__doc_id__ = t2.__doc_id__ AND t1.term < t2.term)
                 GROUP BY 1, 2
              ) t
             ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(COALESCE(CAST(orig AS varchar), '(null)'))) FROM texts.containertrailerplanpaths WHERE orig IN ('75H', '010PM') )
            AS doccount dimension
     )")
  
  
})