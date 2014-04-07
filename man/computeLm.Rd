\name{computeLm}
\alias{computeLm}
\title{Fit Linear Model and return its coefficients.}
\usage{
  computeLm(channel, tableName, expr, where = NULL,
    test = FALSE)
}
\arguments{
  \item{channel}{connection object as returned by
  \code{\link{odbcConnect}}}

  \item{tableName}{Aster table name}

  \item{expr}{an object of class "formula" (or one that can
  be coerced to that class): a symbolic description of the
  model to be fitted. The details of model specification
  are given under `Details`.}

  \item{where}{specifies criteria to satisfy by the table
  rows before applying computation. The creteria are
  expressed in the form of SQL predicates (inside
  \code{WHERE} clause).}

  \item{test}{logical: if TRUE show what would be done,
  only (similar to parameter \code{test} in \link{RODBC}
  functions like \link{sqlQuery} and \link{sqlSave}).}
}
\value{
  Outputs data frame containing 3 columns: \describe{
  \item{coefficient_name}{name of predictor table column,
  zeroth coefficient name is "0"}
  \item{coefficient_index}{index of predictor table column
  starting with 0} \item{value}{coefficient value} }
}
\description{
  Outputs coefficients of the linear model fitted to Aster
  table according to the formula expression containing
  column names. The zeroth coefficient corresponds to the
  slope intercept. R formula expression with column names
  for response and predictor variables is exactly as in
  \code{\link{lm}} function (though less features
  supported).
}
\details{
  Models for \code{computeLm} are specified symbolically. A
  typical model has the form \code{response ~ terms} where
  response is the (numeric) column and terms is a series of
  column terms which specifies a linear predictor for
  response. A terms specification of the form \code{first +
  second} indicates all the terms in first together with
  all the terms in second with duplicates removed. A
  specification of the form \code{first:second} and
  \code{first*second} (interactions) are not supported yet.
}
\examples{
\donttest{

model1 = computeLm(channel=conn, tableName="batting_enh", expr= ba ~ rbi + bb + so)
}
}

