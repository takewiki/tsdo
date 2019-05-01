library(tsdo)
library(usethis)
library(testthat)
context('logical test')
test_that('logical result ',{
  expect_equal(is_all_true(c(T,T,F,T)),F)
  expect_equal(is_all_true(c(T,T,T,T)),T)
  expect_equal(is.na(is_all_true(c(T,T,F,NA))),T)
})