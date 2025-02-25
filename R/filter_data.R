filter_data <- function(df, start_date, end_date) {
  start_date <- as.Date(paste0(start_date, "-01"))
  end_date <- as.Date(paste0(end_date, "-01"))
  
  df_out <- df[df$date >= start_date & df$date <= end_date, ]
  df_out$month_year <- format(as.Date(df_out$date), "%Y-%m")
  
  return(df_out)
}
