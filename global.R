library(bslib)
library(shiny)
library(dplyr)
library(sjmisc)
library(reactable)
library(gitcreds)

source("R/read_data.R")

team_data <- read_team_data()
