test_that("testing wargs", {
  expect_true(!is.na(
    wargs(mean,na.rm=T)(c(NA, 1:3))
    ))  
  paste1 <- wargs(paste, sep='-')
  expect_equal(
    paste1(1, 2, 3)
    , "1-2-3")
})

