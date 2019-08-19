rm(list = ls())
library(shiny)
library("stringr")
source("report_ui.R")
source("data_collection1.R")
source("data_collection2.R")

ui = (htmlOutput("page"))