#' Title
#'
#' Description
#'
#' @param expected 
#' @param actual 
#' @return a summary of some sort
#' @examples
#' \dontrun{
#' library(verifyImpl)
#' }
#' @export
compareSeries <- function(expected, actual) {
  stop("Not yet implemented")
}

#' Intersect two data frames (typically time series) by common column names
#'
#' Intersect two data frames (typically time series) by common column names. Useful when two algorithm implementation output different trace of state variables
#'
#' @param first a series/data frame
#' @param second a series/data frame
#' @return a list with names 'first' and 'second', and values are series with common column names.
#' @examples
#' \dontrun{
#' library(verifyImpl)
#' }
#' @export
intersectSeries <- function(first, second) {
  common <- intersect(names(first), names(first))
  list(first=first[,common], second=second[,common])
}

#' Rename the column names of a data frame, using a lookup table
#'
#' Rename the column names of a data frame, using a lookup table
#'
#' @param series the data frame with column names to rename.
#' @param namesLookup a list, lookup table with names and values as characters
#' @return series with renamed column names
#' @examples
#' \dontrun{
#' library(verifyImpl)
#' }
#' @export
renameSeries <- function(series, namesLookup) {
  # TODO consider using http://cran.csiro.au/web/packages/hash/index.html
  stopifnot(is.list(namesLookup))
  x <- names(series)
  if(is.null(x)) { stop('the series argument is not named') }
  y <- sapply(x, FUN=function(a){ b <- namesLookup[[a]] ; if (is.null(b)) a else b })
  names(series) <- y
  return(series)
}

#' Extract data side by side from two time series 
#'
#' Extract data side by side from two time series 
#'
#' @param expected a time series (e.g. xts). Suggests to use this for the expected or observed data set
#' @param actual  a time series (e.g. xts). Suggests to use this for the actual data set that should match the expected one.
#' @param varId a character vector of length 1, the name of the series to identify
#' @param tSpan a vector of length 2, coercible to a POSIXct, or NA (default value) to not temporally subset the returned time series.
#' @return series with renamed column names
#' @examples
#' \dontrun{
#' library(verifyImpl)
#' }
#' @export
extractSbS <- function(expected, actual, varId, tSpan=NA) {
  stopifnot(length(varId)==1)
  stopifnot(varId %in% names(expected))
  stopifnot(varId %in% names(actual))
  before <- expected[,varId]
  after <- actual[,varId]
  z <- merge(before, after)
  if(!is.na(tSpan[1])) {
    tSpan <- as.POSIXct(tSpan)
    z <- window(z, start=tSpan[1], end=tSpan[2])
  }
  z
}

#' Extract and plot data side by side from two time series 
#'
#' Extract and plot data side by side from two time series 
#'
#' @param expected a time series (e.g. xts). Suggests to use this for the expected or observed data set
#' @param actual  a time series (e.g. xts). Suggests to use this for the actual data set that should match the expected one.
#' @param varId a character vector of length 1, the name of the series to identify
#' @param tSpan a vector of length 2, coercible to a POSIXct, or NA (default value) to not temporally subset the returned time series.
#' @param title title to give to the resulting plot
#' @return a plot object
#' @examples
#' \dontrun{
#' library(verifyImpl)
#' }
#' @export
plotCompare <- function(expected, actual, varId, tSpan=NA, title=paste("before/after state trace", varId)) {
  z <- extractSbS(expected, actual, varId, tSpan)
  plot.zoo(z, plot.type='single', col=c('blue','red'), main=title)
}