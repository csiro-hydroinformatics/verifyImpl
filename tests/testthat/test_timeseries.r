context("time series")

# setwd('F:/src/github_jm/verifyImpl/tests')
# source( 'testthat.r')

mkDate <- function (year, month, day, hour = 0, min = 0, sec = 0, tz = "UTC")  { ISOdate(year, month, day, hour, min, sec, tz=tz) }


mkTestTs <- function(tStepSec=3600) {
  sts <- 1:10
  testts <- data.frame(a=sts,b=sts*1.1,c=sts*1.2,d=sts*1.3,e=sts*1.4)
  s <- mkDate(2001, 01, 02)
  xts(testts, s + (1:length(sts)*tStepSec))
}

test_that("Time series renamed correctly", {
  s <- mkTestTs()
  lookup <- list(a='Aa', b='Bb', d='Dd', otherName='otherName')
  actual <- renameSeries(s, lookup)
  expect_equal( c("Aa", "Bb", 'c', "Dd", 'e'), names(actual))
})
