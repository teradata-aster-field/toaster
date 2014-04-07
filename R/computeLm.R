#' Compute Linear Regression model coefficients
#' 
#' Outputs coefficients of the linear model fitted to Aster table according
#' to the formula expression with column names. The zeroth coefficient corresponds 
#' to the slope intercept. R formula expression with column names for response and 
#' predictor variables is exactly as in \code{\link{lm}} function (though less 
#' features supported).
#' 
#' Models for \code{computeLm} are specified symbolically. A typical model has the form 
#' \code{response ~ terms} where response is the (numeric) column and terms is a series of
#' column terms which specifies a linear predictor for response. A terms specification of 
#' the form \code{first + second} indicates all the terms in first together with all the 
#' terms in second with duplicates removed. A specification of the form \code{first:second} 
#' and \code{first*second} (interactions) are not supported yet.
#' 
#' @param channel connection object as returned by \code{\link{odbcConnect}}
#' @param tableName Aster table name
#' @param expr an object of class "formula" (or one that can be coerced to that class): 
#'   a symbolic description of the model to be fitted. The details of model 
#'   specification are given under ‘Details’.
#' @param where specifies criteria to satisfy by the table rows before applying
#'   computation. The creteria are expressed in the form of SQL predicates (inside
#'   \code{WHERE} clause).
#' @param test logical: if TRUE show what would be done, only (similar to parameter \code{test} in \link{RODBC} 
#'   functions like \link{sqlQuery} and \link{sqlSave}).  
#' @return
#' Outputs data frame containing 3 columns:
#' \describe{
#'   \item{coefficient_name}{name of predictor table column, zeroth coefficient name is "0"}
#'   \item{coefficient_index}{index of predictor table column starting with 0}
#'   \item{value}{coefficient value}
#' }
#' @export
#' @examples
#' \donttest{
#' 
#' model1 = computeLm(channel=conn, tableName="batting_enh", expr= ba ~ rbi + bb + so)
#' }
#' 
computeLm <- function(channel, tableName, expr, where = NULL, 
                          test = FALSE) {
  
  if (missing(channel)) {
    stop("Must provide connection.")
  }
  
  if (missing(tableName) || missing(expr)) {
    stop("Must provide table and expression.")
  }
    
  ft = terms(expr)
  #fvars = all.vars(expr)
  vars = as.character(attr(ft, "variables"))[-1]
  if (length(vars) < 2) {
    stop("No predictors found in formula.")
  }
  
  responseIdx = attr(ft, "response")
  if (responseIdx < 1) {
    stop("No response variable found in formula.")
  }
  responseVar = vars[[responseIdx]]
  
  where_clause = makeWhereClause(where)
    
  predictors = vars[-responseIdx]
  predictorList = paste0(predictors, rep(" x", length(predictors)), as.character(seq(1, length(predictors))), collapse=", ")
  selectList = paste0(predictorList, ", ", responseVar, " y ")
  
  sql = paste0(
        "SELECT * 
           FROM linreg(
                  ON linregmatrix(
                    ON (SELECT ", selectList, " FROM ", tableName, where_clause, ")
                  )
                  PARTITION BY 1
           )"
  )
  
  if (test) 
    return (sql)
  else {
    result = sqlQuery(channel, sql, stringsAsFactors=FALSE)
    result = cbind(data.frame(coefficient_name = c("0", predictors)), result)
    return(result)
  }
}

