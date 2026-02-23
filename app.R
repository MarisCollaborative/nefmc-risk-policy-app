# Risk Policy Shiny App ####
# Script reads in the respective files to run the app. This structure avoids running dual database requests in the same file and invalidating the app.

## Load required packages for this script ####
library(shiny)
library(purrr)
library(here)

## Load in app files #### 
# create an object that contains the names of the files needed to run the app
files <- c("helpers.R", "ui.R", "server.R")

# source each of the files and read it into the environment  
purrr::map(files, ~source(here("R", .)))

## Run the App #### 
shiny::shinyApp(ui, server)

