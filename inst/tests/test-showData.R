context("showData")

pitching_info = dget("pitchingInfo.dat")
batting_info = dget("battingInfo.dat")

test_that("format 'boxplot' works", {
  p = ggplot_build(showData(tableName='pitching', tableInfo=pitchingInfo, format='boxplot'))
  
  expect_equal(nrow(p$data[[1]]), length(getNumericColumns(pitchingInfo)))
  expect_equal(setdiff(p$panel$ranges[[1]]$x.labels, getNumericColumns(pitchingInfo)),
               character(0))
})

test_that("format 'boxplot' with facets works", {
  
  cols = c('bb','er','era','so','r')
  p = ggplot_build(showData(tableName='pitching', tableInfo=pitchingInfo, format='boxplot',
               include=cols, facet=TRUE))
  expect_equal(nrow(p$panel$layout), length(cols))
  expect_equal(nrow(p$data[[1]]), length(cols))
  
})

test_that("showData issues warnings", {
  
  # format='boxplot'
  expect_warning(showData(tableInfo=pitching_info, type='character', format='boxplot'),
                 "Automatically regressing to numerical types")
  
  # format='corr'
  expect_warning(showData(tableInfo=pitching_info, type='character', format='corr'),
                 "Ignoring non-numeric types")
})


test_that("showData throws errors", {
  
  # format='boxplot'
  expect_error(showData(tableInfo=batting_info, type='numeric', format='boxplot', include='nothing'),
               "Not all specified columns are in the table summary")

  expect_error(showData(tableInfo=batting_info, type='numeric', format='boxplot', 
                        include=c('ab','g','lgid','playerid'), except=c('ab','g')),
               "Nothing to show: check lists of columns")
  
  expect_error(showData(tableInfo=batting_info, type='numeric', format='boxplot', include=c('lgid','playerid')),
               "Nothing to show: check lists of columns")
  
  # format='histogram'
  expect_error(showData(tableInfo=batting_info, type='character', format='histogram'),
               "Factor histograms are not supported")
  expect_error(showData(tableInfo=batting_info, type='temporal', format='histogram'),
               "Datetime histograms are not supported")
  
  # format='scatterplot'
  expect_error(showData(tableInfo=batting_info, format='scatterplot'), 
               "requires sampleFraction or sampleSize specified")
  expect_error(showData(tableInfo=batting_info, format='scatterplot', sampleSize=1, include=c('lgid','playerid')), 
               "numerical data only")
  expect_error(showData(tableInfo=batting_info, format='scatterplot', sampleSize=1, include=c('ba')), 
               "define x and y coordiantes")
  expect_error(showData(tableInfo=batting_info[batting_info$COLUMN_NAME %in% c('lgid')], format='scatterplot', sampleSize=1, 
                        include=c('lgid','playerid')),
               "Not all specified columns are in the table summary")
  
  # format='corr'
  expect_error(showData(tableInfo=batting_info, format='corr', include=c('lgid','playerid')),
               "Nothing to show: check lists of columns")
})