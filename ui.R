rm(list = ls())
library(shiny)
library("stringr")
source("data_collection1.R")
source("data_collection2.R")
source("password_page.R")

ui = (htmlOutput("page"))