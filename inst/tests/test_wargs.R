test_that("testing wargs", {
  expect_true(!is.na(wargs(mean,na.rm=T)(c(NA, 1:3))))  
})