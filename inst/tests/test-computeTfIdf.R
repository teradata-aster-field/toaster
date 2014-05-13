context("computeTfIdf")




test_that("computeTfIdf SQL is correct", {
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="texts.containertrailerplanpaths", docId="orig", 
                 textColumns=c("f5466all", "f5466missent", "drivernamemissent"),
                 parser=ngram(2), test=TRUE),
    "SELECT * FROM TF_IDF(
       ON TF(
         ON (SELECT docid, term FROM (SELECT orig docid, ngram as term, frequency 
       FROM nGram (
         ON (SELECT orig, f5466all || ' ' || f5466missent || ' ' || drivernamemissent __text_column__  FROM texts.containertrailerplanpaths  )      TEXT_COLUMN('__text_column__')        GRAMS(2) 
         OVERLAPPING('true')
         CASE_INSENSITIVE('false')        ACCUMULATE('orig')
       ) ) t) PARTITION BY docid
          ) AS TF PARTITION BY term
       ON ( SELECT COUNT(DISTINCT(orig)) FROM texts.containertrailerplanpaths )
            AS doccount dimension
     )"
    )
  
  expect_equal_normalized(
    computeTfIdf(channel=NULL, tableName="texts.containertrailerplanpaths", docId="orig",
                 textColumns=c("commentsall"), 
                 parser=ngram(2), where="orig IN ('75H', '010PM')", test=TRUE),
    " "
    )
})