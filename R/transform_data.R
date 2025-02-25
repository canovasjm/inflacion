transform_data <- function(df) {
  df$cumulative_inflation <- cumprod(1 + (as.numeric(df$inflation) / 100)) - 1
  
  return(df)
}
