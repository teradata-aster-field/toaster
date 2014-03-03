
#' Normalize text to remove repeating white spaces across multiple lines
replaceWhite <- function(s) {
  gsub("\\s+", " ", s)
}

#' Extension of \code{\link{expect_equal}} to compare normalized texts
expect_equal_normalized <- function(object, expected, info = NULL, label = NULL) {
  
  expect_equal(replaceWhite(object), replaceWhite(expected), info=info, label=label)
  
}