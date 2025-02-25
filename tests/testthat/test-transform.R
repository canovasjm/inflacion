#source("../../R/transform_data.R")

library(testthat)

test_that("transform_data correctly transforms", {
  inflation_data <- data.frame(
    date = as.Date(c("2023-01-01", "2023-02-01", "2023-03-01", "2023-04-01")),
    inflation = c(6.0, 6.6, 7.7, 8.4)
  )
  
  result <- transform_data(inflation_data)
  
  expect_equal(nrow(result), 4)
  expect_equal(result$cumulative_inflation, c(0.0600000, 0.1299600, 0.2169669, 0.3191921), tolerance = 1e-6)
})
