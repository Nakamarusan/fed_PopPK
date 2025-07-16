library(testthat)
source("//root/test/server/R/server_objective.R")
test_that("aggregate_responses sums objf and grad correctly", {
  # three clients, each with same-length grad
  resp_list <- list(
    list(objf = 10, grad = c(1, 2, 3)),
    list(objf = 20, grad = c(4, 5, 6)),
    list(objf = 30, grad = c(7, 8, 9))
  )

  out <- aggregate_responses(resp_list)
  expect_equal(out$objf, 10 + 20 + 30)
  expect_equal(out$grad, c(1+4+7, 2+5+8, 3+6+9))
})

test_that("aggregate_responses rejects empty or non-list", {
  expect_error(aggregate_responses(NULL),    "`responses` must be a non-empty list")
  expect_error(aggregate_responses(list()),  "`responses` must be a non-empty list")
})

test_that("aggregate_responses checks objf scalar", {
  bad <- list(
    list(objf = c(1,2), grad = c(1,2)),
    list(objf = 5,       grad = c(1,2))
  )
  expect_error(aggregate_responses(bad), "response\\$objf must be a numeric scalar")
})

test_that("aggregate_responses checks grad length consistency", {
  bad <- list(
    list(objf = 5, grad = c(1,2)),
    list(objf = 6, grad = c(1,2,3))
  )
  expect_error(aggregate_responses(bad), "All `grad` vectors must have the same length")
})
