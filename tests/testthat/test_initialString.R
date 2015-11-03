library(stringr)
context("String length")

test_that("1. str_length is number of characters",{
  expect_equal(str_length("a"),1)
  expect_equal(str_length("ab"),2)
  expect_equal(str_length("abc"),3)
})

test_that("2. length of factor is length of level",{
  expect_equal(length(factor("a")),1)
  expect_equal(length(factor("ab")),1)
  expect_equal(length(factor("abc")),1)
})


