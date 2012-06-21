context("Composition")
test_that("Testing compose", {
  expect_is(compose(any, is.na), 'function')
  expect_error(compose(any, isna))
  expect_true(compose(any,is.na)(NA))
  expect_true((any%.%is.na)(NA))
})
