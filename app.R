# Risk Policy Shiny App ####
# Script reads in the respective files to run the app. This structure avoids running dual database requests in the same file and invalidating the app.

## Load required packages for this script ####
library(shiny)
library(purrr)
library(here)

## Load in app files #### 
# # source each of the files and read it into the environment  
source(here("helpers.R"))
source(here("ui.R"))
source(here("server.R"))

## Run the App #### 
shiny::shinyApp(ui, server)

  