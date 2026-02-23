# App ####
library(shiny)
library(tidyverse)
library(here)
library(surveydown)
library(gt)

# Load in the functions, ui, and server into the environment
source(here("helpers.R"))
source(here("ui.R"))
source(here("server.R"))
# files <- c("functions.R", "ui.R", "server.R")
# purrr::map(files, ~source(here("R", .)))

# run the app 
shiny::shinyApp(ui, server)

# shiny::runApp(display.mode="showcase")
