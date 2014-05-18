context("computeTfIdf")



test_that("computeTfIdf SQL with nGram parser is correct", {
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="texts.containertrailerplanpaths", docId="orig", 
                 textColumns=c("f5466all", "f5466missent", "drivernamemissent"),
                 parser=nGram(2), test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( SELECT orig docid, ngram as term, frequency 
                    FROM nGram (
                      ON (SELECT orig, f5466all || ' ' || f5466missent || ' ' || drivernamemissent __text_column__  
                            FROM texts.containertrailerplanpaths )      
                      TEXT_COLUMN('__text_column__')        
                      GRAMS(2) 
                      OVERLAPPING('true')
                      CASE_INSENSITIVE('false')        
                      ACCUMULATE('orig')
                    ) 
               ) t
            ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(orig)) FROM texts.containertrailerplanpaths )
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
               FROM ( SELECT orig docid, ngram as term, frequency 
                    FROM nGram (
                      ON (SELECT orig, commentsall __text_column__  
                            FROM texts.containertrailerplanpaths WHERE orig IN ('75H', '010PM') ) 
                      TEXT_COLUMN('__text_column__')        
                      GRAMS(2) 
                      OVERLAPPING('true')
                      CASE_INSENSITIVE('false')   
                      ACCUMULATE('orig')
                    ) 
               ) t
            ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(orig)) FROM texts.containertrailerplanpaths WHERE orig IN ('75H', '010PM') )
            AS doccount dimension
     )"
    )
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="texts.containertrailerplanpaths", docId="orig", 
                 textColumns=c("f5466all", "f5466missent", "drivernamemissent"),
                 parser=nGram(2:4), test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( SELECT orig docid, ngram as term, frequency 
                        FROM nGram (
                          ON (SELECT orig, f5466all || ' ' || f5466missent || ' ' || drivernamemissent __text_column__  
                                FROM texts.containertrailerplanpaths )      
                          TEXT_COLUMN('__text_column__')        
                          GRAMS(2) 
                          OVERLAPPING('true')
                          CASE_INSENSITIVE('false')        
                          ACCUMULATE('orig')
                    ) 
                    UNION ALL
                      SELECT orig docid, ngram as term, frequency 
                        FROM nGram (
                          ON (SELECT orig, f5466all || ' ' || f5466missent || ' ' || drivernamemissent __text_column__  
                                FROM texts.containertrailerplanpaths )      
                          TEXT_COLUMN('__text_column__')        
                          GRAMS(3) 
                          OVERLAPPING('true')
                          CASE_INSENSITIVE('false')        
                          ACCUMULATE('orig')
                    )
                    UNION ALL
                      SELECT orig docid, ngram as term, frequency 
                        FROM nGram (
                          ON (SELECT orig, f5466all || ' ' || f5466missent || ' ' || drivernamemissent __text_column__  
                                FROM texts.containertrailerplanpaths )      
                          TEXT_COLUMN('__text_column__')        
                          GRAMS(4) 
                          OVERLAPPING('true')
                          CASE_INSENSITIVE('false')        
                          ACCUMULATE('orig')
                   )
                 ) t
            ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(orig)) FROM texts.containertrailerplanpaths )
            AS doccount dimension
     )"
  )
})

test_that("computeTfIdf SQL with token parser is correct", {
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="texts.containertrailerplanpaths", docId="orig", 
                 textColumns=c("f5466all", "f5466missent", "drivernamemissent"),
                 parser=token(1, stopWordsFile="english.dat"), test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( 
                 WITH tokens AS
                   (SELECT *
                      FROM text_parser(
                        ON (SELECT orig, f5466all || ' ' || f5466missent || ' ' || drivernamemissent __text_column__ 
                              FROM texts.containertrailerplanpaths )
                        PARTITION BY orig 
                        TEXT_COLUMN('__text_column__')
                        DELIMITER('[ \\t\\b\\f\\r]+')
                        CASE_INSENSITIVE('false')
                        STEMMING('false')
                        STOP_WORDS('english.dat')
                        ACCUMULATE('orig')
                        TOTAL('false')
                        LIST_POSITIONS('false')
                        TOKEN_COLUMN_NAME('term')
                        FREQUENCY_COLUMN_NAME('frequency')
                        OUTPUT_BY_WORD('true')
                      )
                     WHERE length(term) > 0)
                 SELECT t1.orig docid, term term, COUNT(*) frequency
                   FROM tokens t1
                  GROUP BY 1, 2
               ) t
            ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(orig)) FROM texts.containertrailerplanpaths )
            AS doccount dimension
     )")
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="texts.containertrailerplanpaths", docId="orig", 
                 textColumns=c("f5466all", "f5466missent", "drivernamemissent"),
                 parser=token(2), where="orig IN ('75H', '010PM')", test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( 
                 WITH tokens AS
                   (SELECT *
                      FROM text_parser(
                        ON (SELECT orig, f5466all || ' ' || f5466missent || ' ' || drivernamemissent __text_column__ 
                              FROM texts.containertrailerplanpaths WHERE orig IN ('75H', '010PM') )
                        PARTITION BY orig 
                        TEXT_COLUMN('__text_column__')
                        DELIMITER('[ \\t\\b\\f\\r]+')
                        CASE_INSENSITIVE('false')
                        STEMMING('false')
                        ACCUMULATE('orig')
                        TOTAL('false')
                        LIST_POSITIONS('false')
                        TOKEN_COLUMN_NAME('term')
                        FREQUENCY_COLUMN_NAME('frequency')
                        OUTPUT_BY_WORD('true')
                      )
                    WHERE length(term) > 0)
                SELECT t1.orig docid, t1.term || '+' || t2.term term, COUNT(*) frequency
                  FROM tokens t1 JOIN 
                       tokens t2 ON (t1.orig = t2.orig AND t1.term < t2.term)
                 GROUP BY 1, 2
              ) t
             ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(orig)) FROM texts.containertrailerplanpaths WHERE orig IN ('75H', '010PM') )
            AS doccount dimension
     )")
  
  
})