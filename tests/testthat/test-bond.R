library(israelcurves)
context("bond function")



#check that CF of bonds is equal
CF_equal <- function(bond1,bond2) {
  all.equal(bond1$dates,bond2$dates) & all.equal(bond1$payments,bond2$payments)
}

test_that("cashflow with sorted dates is the same as bond with not sorted dates", {

  #build two bonds with same CF and dates but not the same order
  ind <- 1:5
  ind2 <- sample(ind)
  dates1 <- seq(as.Date("2000-01-01"),length=5,by="1 year")
  CF1 <- c(5,5,5,5,105)
  dates2 <- dates1[ind2]
  CF2 <- CF1[ind2]
  #dates1 - sorted dates bond, dates2 - unsorted dates bond

  expect_true(CF_equal(bond(dates1,CF1),bond(dates2,CF2)))
  expect_warning(bond(dates2,CF2))
})
