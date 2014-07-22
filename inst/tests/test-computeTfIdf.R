context("computeTfIdf")



test_that("computeTfIdf SQL with nGram parser is correct", {
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="public.dallaspoliceall", docId="substr(offensezip, 1, 4)", 
                 textColumns=c("offensedescription", "offensenarrative"),
                 parser=nGram(2), test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( SELECT __doc_id__ docid, ngram as term, frequency 
                    FROM nGram (
                      ON (SELECT COALESCE(CAST(substr(offensezip, 1, 4) AS varchar), '(null)') __doc_id__, 
                                 offensedescription || ' ' || offensenarrative __text_column__  
                            FROM public.dallaspoliceall )      
                      TEXT_COLUMN('__text_column__')  
                      DELIMITER('[ \\t\\b\\f\\r]+')
                      GRAMS(2) 
                      OVERLAPPING('true')
                      CASE_INSENSITIVE('false')        
                      ACCUMULATE('__doc_id__')
                    )
               WHERE length(ngram) >= 3
               ) t
            ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(COALESCE(CAST(substr(offensezip, 1, 4) AS varchar), '(null)'))) FROM public.dallaspoliceall )
            AS doccount dimension
     )"
    )
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="public.dallaspoliceall", docId="substr(offensezip, 1, 4)", 
                 textColumns=c("offensedescription", "offensenarrative"),
                 parser=nGram(3, minLength=5, delimiter="[ -]+", overlapping=FALSE, ignoreCase=TRUE,
                              punctuation="[-\\\\\\[.,?\\!:;~()\\\\\\]]+", reset="[#]+", sep="###"), 
                 test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( SELECT __doc_id__ docid, ngram as term, frequency 
                    FROM nGram (
                      ON (SELECT COALESCE(CAST(substr(offensezip, 1, 4) AS varchar), '(null)') __doc_id__, 
                                 offensedescription || '###' || offensenarrative __text_column__  
                            FROM public.dallaspoliceall )      
                      TEXT_COLUMN('__text_column__')   
                      DELIMITER('[ -]+')
                      GRAMS(3) 
                      OVERLAPPING('false')
                      CASE_INSENSITIVE('true')
                      PUNCTUATION('[-\\\\\\[.,?\\!:;~()\\\\\\]]+')
                      RESET('[#]+')
                      ACCUMULATE('__doc_id__')
                    )
               WHERE length(ngram) >= 17
               ) t
            ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(COALESCE(CAST(substr(offensezip, 1, 4) AS varchar), '(null)'))) FROM public.dallaspoliceall )
            AS doccount dimension
     )"
  )
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="public.dallaspoliceall", docId=c("substr(offensezip, 1, 3)", "offensecity"), 
                 idSep = '***', idNull = '<n-u-l-l>',
                 textColumns=c("offensedescription", "offensenarrative"),
                 parser=nGram(2), test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( SELECT __doc_id__ docid, ngram as term, frequency 
                    FROM nGram (
                      ON (SELECT COALESCE(CAST(substr(offensezip, 1, 3) AS varchar), '<n-u-l-l>') || '***' || COALESCE(CAST(offensecity AS varchar), '<n-u-l-l>') __doc_id__, 
                                 offensedescription || ' ' || offensenarrative __text_column__  
                            FROM public.dallaspoliceall )      
                      TEXT_COLUMN('__text_column__')      
                      DELIMITER('[ \\t\\b\\f\\r]+')
                      GRAMS(2) 
                      OVERLAPPING('true')
                      CASE_INSENSITIVE('false')        
                      ACCUMULATE('__doc_id__')
                    ) 
              WHERE length(ngram) >= 3
               ) t
            ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(COALESCE(CAST(substr(offensezip, 1, 3) AS varchar), '<n-u-l-l>') || '***' || COALESCE(CAST(offensecity AS varchar), '<n-u-l-l>'))) 
                FROM public.dallaspoliceall )
            AS doccount dimension
     )"
  )
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="public.dallaspoliceall", docId="offensezip",
                 textColumns=c("offensedescription"), 
                 parser=nGram(2), where="offensezip like '75%'", test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( SELECT __doc_id__ docid, ngram as term, frequency 
                    FROM nGram (
                      ON (SELECT COALESCE(CAST(offensezip AS varchar), '(null)') __doc_id__, offensedescription __text_column__  
                            FROM public.dallaspoliceall WHERE offensezip like '75%' ) 
                      TEXT_COLUMN('__text_column__')     
                      DELIMITER('[ \\t\\b\\f\\r]+')
                      GRAMS(2) 
                      OVERLAPPING('true')
                      CASE_INSENSITIVE('false')   
                      ACCUMULATE('__doc_id__')
                    ) 
               WHERE length(ngram) >= 3
               ) t
            ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(COALESCE(CAST(offensezip AS varchar), '(null)'))) FROM public.dallaspoliceall 
               WHERE offensezip like '75%' )
            AS doccount dimension
     )"
    )
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="public.dallaspoliceall", docId="offensegender", 
                 textColumns=c("offensedescription", "offensenarrative", "offenseweather"),
                 parser=nGram(2:4), test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( SELECT __doc_id__ docid, ngram as term, frequency 
                        FROM nGram (
                          ON (SELECT COALESCE(CAST(offensegender AS varchar), '(null)') __doc_id__, 
                                     offensedescription || ' ' || offensenarrative || ' ' || offenseweather __text_column__  
                                FROM public.dallaspoliceall )      
                          TEXT_COLUMN('__text_column__') 
                          DELIMITER('[ \\t\\b\\f\\r]+')
                          GRAMS(2) 
                          OVERLAPPING('true')
                          CASE_INSENSITIVE('false')        
                          ACCUMULATE('__doc_id__')
                        )
                       WHERE length(ngram) >= 3
                    UNION ALL
                      SELECT __doc_id__ docid, ngram as term, frequency 
                        FROM nGram (
                          ON (SELECT COALESCE(CAST(offensegender AS varchar), '(null)') __doc_id__, 
                                     offensedescription || ' ' || offensenarrative || ' ' || offenseweather __text_column__  
                                FROM public.dallaspoliceall )      
                          TEXT_COLUMN('__text_column__') 
                          DELIMITER('[ \\t\\b\\f\\r]+')
                          GRAMS(3) 
                          OVERLAPPING('true')
                          CASE_INSENSITIVE('false')        
                          ACCUMULATE('__doc_id__')
                        )
                       WHERE length(ngram) >= 5
                    UNION ALL
                      SELECT __doc_id__ docid, ngram as term, frequency 
                        FROM nGram (
                          ON (SELECT COALESCE(CAST(offensegender AS varchar), '(null)') __doc_id__, 
                                     offensedescription || ' ' || offensenarrative || ' ' || offenseweather __text_column__  
                                FROM public.dallaspoliceall )      
                          TEXT_COLUMN('__text_column__')   
                          DELIMITER('[ \\t\\b\\f\\r]+')
                          GRAMS(4) 
                          OVERLAPPING('true')
                          CASE_INSENSITIVE('false')        
                          ACCUMULATE('__doc_id__')
                        )
                       WHERE length(ngram) >= 7
                 ) t
            ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(COALESCE(CAST(offensegender AS varchar), '(null)'))) FROM public.dallaspoliceall )
            AS doccount dimension
     )"
  )
})

test_that("computeTfIdf SQL with token parser is correct", {
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="public.dallaspoliceall", docId="offensestatus", 
                 textColumns=c("offensedescription", "offensenarrative", "offenseweather"),
                 parser=token(1, stopWords="english.dat"), test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( 
                 WITH tokens AS
                   (SELECT *
                      FROM text_parser(
                        ON (SELECT COALESCE(CAST(offensestatus AS varchar), '(null)') __doc_id__, 
                                   offensedescription || ' ' || offensenarrative || ' ' || offenseweather __text_column__ 
                              FROM public.dallaspoliceall )
                        PARTITION BY __doc_id__ 
                        TEXT_COLUMN('__text_column__')
                        DELIMITER('[ \\t\\b\\f\\r]+')
                        CASE_INSENSITIVE('false')
                        STEMMING('false')
                        REMOVE_STOP_WORDS('true')
                        STOP_WORDS('english.dat')
                        ACCUMULATE('__doc_id__')
                        TOTAL('false')
                        LIST_POSITIONS('false')
                        TOKEN_COLUMN_NAME('term')
                        FREQUENCY_COLUMN_NAME('frequency')
                        OUTPUT_BY_WORD('true')
                      )
                     WHERE length(term) >= 1)
                 SELECT t1.__doc_id__ docid, term term, COUNT(*) frequency
                   FROM tokens t1
                  GROUP BY 1, 2
               ) t
            ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(COALESCE(CAST(offensestatus AS varchar), '(null)'))) FROM public.dallaspoliceall )
            AS doccount dimension
     )")
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="public.dallaspoliceall", docId="offensestatus", 
                 textColumns=c("offensedescription", "offensenarrative", "offenseweather"),
                 parser=token(2), where="offensestatus NOT IN ('System.Xml.XmlElement', 'C')", test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term 
               FROM ( 
                 WITH tokens AS
                   (SELECT *
                      FROM text_parser(
                        ON (SELECT COALESCE(CAST(offensestatus AS varchar), '(null)') __doc_id__, 
                                   offensedescription || ' ' || offensenarrative || ' ' || offenseweather __text_column__ 
                              FROM public.dallaspoliceall WHERE offensestatus NOT IN ('System.Xml.XmlElement', 'C') )
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
                    WHERE length(term) >= 1)
                SELECT t1.__doc_id__ docid, t1.term || '+' || t2.term term, COUNT(*) frequency
                  FROM tokens t1 JOIN 
                       tokens t2 ON (t1.__doc_id__ = t2.__doc_id__ AND t1.term < t2.term)
                 GROUP BY 1, 2
              ) t
             ) PARTITION BY docid
          ) AS TF PARTITION BY term
         ON ( SELECT COUNT(DISTINCT(COALESCE(CAST(offensestatus AS varchar), '(null)'))) FROM public.dallaspoliceall 
               WHERE offensestatus NOT IN ('System.Xml.XmlElement', 'C') )
            AS doccount dimension
     )")
  
  
})