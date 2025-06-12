library(readxl)

message("Start processing...")

# Get the previous month
today <- Sys.Date()

message("Today is: ", today)

# Extract month and year components
mm <- format(today, "%m")
yy <- format(today, "%y") # Last 2 digits of year

# Construct file name and URL
file_name <- sprintf("sh_ipc_%s_%s.xls", mm, yy)
message("File name is: ", file_name)

url <- paste0("https://www.indec.gob.ar/ftp/cuadros/economia/", file_name)
message("URL for download is: ", url)

# Try downloading the file
tryCatch(
  {
    download.file(url, file_name, mode = "wb")
    # Try reading the file to check it's a valid Excel file
    tryCatch(
      {
        read_excel(file_name, n_max = 1)
        message("Download and validation successful: ", file_name)
      },
      error = function(e) {
        message(
          "File is not a valid Excel file. Exiting without error."
        )
        file.remove(file_name)
        quit(save = "no", status = 0)
      }
    )
  },
  error = function(e) {
    message("Download failed: ", e$message)
    quit(save = "no", status = 1)
  },
  warning = function(w) {
    message("Download warning: ", w$message)
    quit(save = "no", status = 1)
  }
)

# Read the Excel file
raw <- read_excel(
  file_name,
  sheet = "Variación mensual IPC Nacional",
  skip = 5,
  n_max = 4
)

# Remove first column
raw <- raw[, -1]

# Remove empty rows
data_rows <- raw[-c(1:3), ]

# Generate monthly dates starting from 2017-01-01
start_date <- as.Date("2017-01-01")
dates <- seq(from = start_date, by = "month", length.out = length(raw))

# Flatten to a tidy data frame
df <- data.frame(
  date = dates,
  inflation = as.numeric(unlist(data_rows[1, ]))
)

# Remove NA rows (just in case)
df <- df[!is.na(df$inflation), ]

message("Row for max date in new data frame: \n")
print(tail(df[nrow(df), ]))

# Read inflation_data from repo
inflation_data <- read.csv("inflation_data.csv")
inflation_data$date <- as.Date(inflation_data$date)
#inflation_data <- inflation_data[-nrow(inflation_data), ]

# Compare max dates between data frames
if (max(df$date) > max(inflation_data$date)) {
  inflation_data <- rbind(inflation_data, df[nrow(df), ])
  message("Row appended to inflation_data \n")

  message("inflation_data tail is: \n")
  print(tail(inflation_data))

  write.csv(inflation_data, "inflation_data.csv", row.names = FALSE)
  message("File: inflation_data.csv updated. \n")
} else {
  message("Max date is the same between data frames. No action needed.\n")
}

message("Finish processing.")
