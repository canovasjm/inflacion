#source("../../R/filter_data.R")

library(testthat)

test_that("filter_data correctly filters", {
  inflation_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01")),
    value = c(6.0, 6.6, 7.7, 8.4)
  )
  
  result <- filter_data(inflation_data, "2023-02", "2023-03")
  
  expect_equal(nrow(result), 2)
  expect_equal(result$date, as.Date(c("2023-02-01", "2023-03-01")))
  expect_equal(result$month_year, c("2023-02", "2023-03"))
})
