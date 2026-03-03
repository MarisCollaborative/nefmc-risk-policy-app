# User Interface for Risk Policy Shiny App ####
## Environment set-up
library(shiny)
library(tidyverse)
library(here)
library(bslib)
library(nefishr)
library(gt)
library(surveydown)
library(shinyjs)

# connect to survey database
# db <- sd_db_connect()

# fetch the risk policy scores from the database 
# data <- sd_get_data(db, "rp-scores")

# identify the unique values of report year that occur in the table
# year_vals <- unique(data$report_year) |> sort()
year_vals <- readRDS(here("data", "year_vals.rds"))

# identify the unique values of Stock Name from the nefmc_species dataframe in the nefishr package
stock_vals <- unique(nefishr::nefmc_species$STOCK_NAME) |> sort()

# identify the unique values of FMP from the nefmc_species dataframe in the nefishr package
fmp_vals <- unique(nefishr::nefmc_species$FMP) |> sort()

# User Interface ####
ui <- fluidPage(
  shinyjs::useShinyjs(),
  # create a multi-page application 
  page_navbar(title = 'NEFMC Risk Policy Application', # with the following title
              # a shared sidebar across pages, that has the following
              sidebar = sidebar( 
                # Settings  
                id = "sidebar", 
                width = 350, 
                open = T, # default to open
                # Instructions:
                p(em("Use the dropdown menus to select parameters for filtering data throughout the application:")),
                # Inputs: 
                ## Year selection 
                selectInput(inputId = "year", 
                      label = strong("Action Year"), 
                      choices = c("Select a year...", year_vals), # using the unique year values from the scoring table
                      selected = "Select a year..."), # the initial value shown when the user starts the app 

                ## FMP selection
                selectInput(inputId = 'fmp', 
                      label = strong("Select FMP"), 
                      choices = c("Select an FMP...", fmp_vals), # using the unique fmp values from the nefmc_species table
                      selected = "Select an FMP..."), # the initial value shown when the user starts the app 
                
                ## Stock selection
                selectInput(inputId = 'stock', 
                            label = strong("Select stock"), 
                            choices = c("Select a stock...", stock_vals), # using the unique stock values from the nefmc_species table
                            selected = "Select a stock..."), # the initial value shown when the user starts the app
                br(), 
                # Generate Report button
                downloadButton("report", "Generate report")
                  # uiOutput("downloadReport")
                ),
              # Page 1 - shows the matrix table based on the sidebar inputs
              nav_panel(title = "Matrix",
                        gt_output("matrix")
                        ), 
              # Page 2 - shows the recommended probability information based on user inputs and contains
              nav_panel(title = "Recommended Probability",
                    # a multi-tab card with  
                    navset_card_tab( 
                        # a shared sidebar to change the factors by one level 
                        sidebar = sidebar(id = "changeFactors",
                        # Instructions
                        p(em("Use the sliders to increase or decrease scores of respective factors. When sliders are ready, click 'Make Changes' to change the factor scores, and resulting Z-score and recommended probability. Reset scores back to their original values and sliders to 0 by clicking 'Reset Scores.'")),
                        sliderInput('changeBiomass', "Biomass", min = -1, max = 1, value = 0, step = 1),
                        sliderInput('changeRecruitment', "Recruitment", min = -1, max = 1, value = 0, step = 1),
                        sliderInput('changeClimate', "Climate Vulnerability", min = -1, max = 1, value = 0, step = 1),
                        sliderInput('changeCommercial', "Commercial Fishery", min = -1, max = 1, value = 0, step = 1),
                        sliderInput('changeRecreational', "Recreational Fishery", min = -1, max = 1, value = 0, step = 1), 
                        # action buttons to manipulate the scores based on user input or restore the scores to their original values
                        actionButton('changeScores', "Make Changes"), 
                        actionButton('resetScores', "Reset Scores")
                                          ),
                        # a table of data with PDT scores and Council weightings, and text containing the calculated z-score and recommended probabilities
                        nav_panel("Z-score Data", 
                                  gt_output("scores"),
                                  p(strong("The Z-score value based on the scores and weights table above:")),
                                  verbatimTextOutput('zscore', placeholder = T), 
                                  p(strong("The recommended probability based on the calculated z-score:")),
                                  verbatimTextOutput('AlphaProb', placeholder = T)), 
                        # a plot of the calculated Z-score and recommended probability based on the scores and weights
                        nav_panel("Alpha Z-score Plot", plotOutput("alpha_plot")),
                        nav_panel("Beta Z-score Plot", 
                                  plotOutput("ab_plot"),
                                  p(strong("The recommended probability based on an updated logisitic curve:")),
                                  verbatimTextOutput('BetaProb', placeholder = T),
                                  p(strong("The percent difference between recommended probabilities based on the two logistic curves:")),
                                  verbatimTextOutput('PercDiff', placeholder = T))
                                        ),
                  # and a text area for user input if there is a decision to change a score(s) and ultimately the recommended probability for a given stock in a given year
                  card(textAreaInput(inputId = "rationale", 
                                     label = "If you are recommending a change to a score, enter your rationale below.", 
                                     width = '100%')#, 
                                     #value = "No changes or additional rationale provided.")
                      )
                
            )
          )
        )
