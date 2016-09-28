library(israelcurves)
context("bond cashflow at a current date")

#build a bond to work with
dates1 <- seq(as.Date("2010-03-31"),length=10,by="1 year")
CF1 <- c(rep(5,9),105)
bond1 <- bond(dates1,CF1,name="testbond",issue_date = as.Date("2009-12-13"))

#return the term for the first payment
first_payment <- function(bond1,calc_date,ex_day) {
  positive_CF(bond1,calc_date,ex_day)$pos_terms[1]
}


test_that("Ex day removes the cashflow if it is the same month after the Ex day", {
  calc_date <- as.Date("2011-03-25")
  expect_equal(first_payment(bond1,calc_date,ex_day=20),as.numeric((dates1[3]-calc_date)/365))
})

test_that("Ex day does not removes the cashflow if it is another month after the Ex day", {
  calc_date <- as.Date("2011-02-25")
  expect_equal(first_payment(bond1,calc_date,ex_day=20),as.numeric((dates1[2]-calc_date)/365))
})

test_that("Ex day does not removes the cashflow if it is another month before the Ex day", {
  calc_date <- as.Date("2011-04-01")
  expect_equal(first_payment(bond1,calc_date,ex_day=20),as.numeric((dates1[3]-calc_date)/365))
})

test_that("Ex day does not removes the cashflow if it is same month before the Ex day", {
  calc_date <- as.Date("2011-03-15")
  expect_equal(first_payment(bond1,calc_date,ex_day=20),as.numeric((dates1[2]-calc_date)/365))
})
