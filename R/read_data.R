library(googlesheets4)
library(googledrive)
library(httpuv)

gs4_auth(cache = ".secrets")

read_team_data <- function() {
  sheet_url <- "https://docs.google.com/spreadsheets/d/1oZ8LJ4gGy5A3uj8mYM6USyH_QEeeOePFw_nmk8kq_BQ"
  googlesheets4::read_sheet(sheet_url)
}
