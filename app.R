# App ####
library(shiny)
library(tidyverse)
library(here)
library(surveydown)
source(here("helpers.R"))

# Load in the functions, ui, and server into the environment
# files <- c("functions.R", "ui.R", "server.R")
# purrr::map(files, ~source(here("R", .)))

# run the app 
# shiny::shinyApp(ui, server)

source(here("ui.R"))
source(here("server.R"))


# run the app 
shiny::shinyApp(ui, server)

# shiny::runApp(display.mode="showcase")
