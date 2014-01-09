context("computeBarchart")

test_that("computeBarchart throws errors", {
  
  expect_error(computeBarchart(channel=NULL), 
               "Must have table name.")
  
  expect_error(computeBarchart(channel=NULL, tableName="fake"),
               "'category' is missing")
  
  expect_error(computeBarchart(channel=NULL, tableName="fake", category=c("1", "2")),
               "Bar chart must have exactly one category.")
  
})


test_that("computeBarchart SQL is correct", {
  
  expect_equal(replaceWhite(
               computeBarchart(channel=NULL, tableName="pitching", category="playerid", test=TRUE)),
               replaceWhite(
                 "SELECT playerid, COUNT(*) cnt
                    FROM pitching
                   GROUP BY playerid ")
               )
  
  expect_equal(replaceWhite(
               computeBarchart(channel=NULL, tableName="pitching", category="playerid", 
                               aggregates=c("AVG(era) avg_era", "AVG(w) avg_w"), where="decadeid between 1980 and 2010",
                               by=c("lgid", "decadeid"), test=TRUE)
               ),
               replaceWhite(
                 "SELECT playerid, lgid, decadeid, AVG(era) avg_era, AVG(w) avg_w 
                    FROM pitching 
                   WHERE decadeid between 1980 and 2010 
                   GROUP BY playerid, lgid, decadeid ")
               )
  
  expect_equal(replaceWhite(
    computeBarchart(channel=NULL, tableName="pitching", category="playerid", 
                    aggregates=c("AVG(era) avg_era", "AVG(w) avg_w"), where="decadeid between 1980 and 2010",
                    test=TRUE)
  ),
               replaceWhite(
                 "SELECT playerid, AVG(era) avg_era, AVG(w) avg_w 
                    FROM pitching 
                   WHERE decadeid between 1980 and 2010 
                   GROUP BY playerid ")
  )
  
  
})

test_that("computeBarchar with Order by and Limit is correct", {
  
  expect_equal(replaceWhite(
    computeBarchart(channel=NULL, tableName="pitching", category="playerid", 
                    orderBy=c('decadeid'), top=100, test=TRUE)),
              replaceWhite(
                "SELECT playerid, COUNT(*) cnt
                   FROM pitching
                  GROUP BY playerid
                  ORDER BY decadeid
                  LIMIT 100")
              )
  
  expect_equal(replaceWhite(
    computeBarchart(channel=NULL, tableName="pitching", category="playerid", 
                    aggregates=c("AVG(era) avg_era", "AVG(w) avg_w"), where="decadeid between 1980 and 2010",
                    by=c("lgid", "decadeid"), 
                    orderBy=c('decadeid'), top=100, test=TRUE)
  ),
               replaceWhite(
                 "SELECT playerid, lgid, decadeid, AVG(era) avg_era, AVG(w) avg_w 
                    FROM pitching 
                   WHERE decadeid between 1980 and 2010 
                   GROUP BY playerid, lgid, decadeid
                   ORDER BY decadeid
                   LIMIT 100")
  )
  
  expect_equal(replaceWhite(
    computeBarchart(channel=NULL, tableName="pitching", category="playerid", 
                    aggregates=c("AVG(era) avg_era", "AVG(w) avg_w"), where="decadeid between 1980 and 2010",
                    orderBy=c('decadeid'), top=100, test=TRUE)
  ),
               replaceWhite(
                 "SELECT playerid, AVG(era) avg_era, AVG(w) avg_w 
                    FROM pitching 
                   WHERE decadeid between 1980 and 2010 
                   GROUP BY playerid 
                   ORDER BY decadeid
                   LIMIT 100")
  )
})